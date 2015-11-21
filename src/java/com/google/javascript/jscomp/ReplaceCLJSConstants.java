package com.google.javascript.jscomp;
// needs to be in this package because a bunch of stuff we need is package protected

import com.google.javascript.rhino.IR;
import com.google.javascript.rhino.Node;

import java.util.HashMap;
import java.util.Map;

import static clojure.lang.Compiler.munge;

/**
 * Created by zilence on 21/11/15.
 */
public class ReplaceCLJSConstants implements CompilerPass {

    private final AbstractCompiler compiler;

    public ReplaceCLJSConstants(AbstractCompiler compiler) {
        this.compiler = compiler;
    }

    @Override
    public void process(Node externs, Node node) {
        TraverseConstants collector = new TraverseConstants();

        // traverse all inputs that require cljs.core + cljs.core itself
        for (CompilerInput input : compiler.getInputsInOrder()) {
            // FIXME: this parses the inputs to get provides/requires
            if (input.getRequires().contains("cljs.core") || input.getProvides().contains("cljs.core")) {
                NodeTraversal.traverseEs6(compiler, input.getAstRoot(compiler), collector);
            }
        }

        // FIXME: should probably group by module
        // finding the input is plenty of work that should probably only be done once
        for (ConstantRef ref : collector.constants.values()) {
            JSModule targetModule = ref.module;

            boolean cljsCoreMod = false;
            CompilerInput targetInput = null;

            for (CompilerInput input : targetModule.getInputs()) {
                if (input.getProvides().contains("cljs.core")) {
                    cljsCoreMod = true;
                    targetInput = input;
                    break;
                }
            }

            Node varNode = IR.var(IR.name(ref.name), ref.node);
            if (!cljsCoreMod) {
                Node target = targetModule.getInputs().get(0).getAstRoot(compiler);
                target.addChildToFront(varNode);
            } else {
                Node target = targetInput.getAstRoot(compiler);
                target.addChildToBack(varNode);
            }
        }

        compiler.reportCodeChange();
    }

    public class ConstantRef {
        final String name;
        final String fqn;
        final Node node;
        JSModule module;

        public ConstantRef(String name, String fqn, Node node) {
            this.name = name;
            this.fqn = fqn;
            this.node = node;
            this.module = null;
        }

        public void setModule(JSModule module) {
            if (this.module == null) {
                this.module = module;
            } else if (this.module.equals(module)) {
                // same module
            } else if (compiler.getModuleGraph().dependsOn(module, this.module)) {
                // will already be declared in dependency
            } else {
                this.module = compiler.getModuleGraph().getDeepestCommonDependency(this.module, module);
                if (this.module == null) {
                    throw new IllegalStateException("failed to find common module");
                }
            }
        }
    }

    public class TraverseConstants implements NodeTraversal.Callback {
        // {fqn ConstantRef}
        final Map<String, ConstantRef> constants = new HashMap<>();

        @Override
        public boolean shouldTraverse(NodeTraversal t, Node n, Node parent) {
            return true;
        }

        public void visit(NodeTraversal t, Node n, Node parent) {
            // new cljs.core.Keyword(ns, name, fqn, hash);
            // new cljs.core.Symbol(ns, name, fqn, hash, meta);

            // must check isGetProp, new something['whatever'] blows up getQualifiedName()
            if (n.isNew()
                    // getprop, ns, name, fqn, hash (keyword)
                    // getprop, ns, name, fqn, hash, meta (symbol)
                    && (n.getChildCount() == 5 || n.getChildCount() == 6)
                    && n.getFirstChild().isGetProp() // cljs.core.Keyword NOT new something()
                    && (n.getChildAtIndex(1).isString() || n.getChildAtIndex(1).isNull()) // ns may be null
                    && n.getChildAtIndex(2).isString() // name is never null
                    && n.getChildAtIndex(3).isString() // fqn is never null
                    && n.getChildAtIndex(4).isNumber()) // hash is precomputed
            {
                String typeName = n.getFirstChild().getQualifiedName();

                switch (typeName) {
                    case "cljs.core.Keyword":
                    case "cljs.core.Symbol":
                        String fqn = n.getChildAtIndex(3).getString();
                        String constantName = "cljs$cst$" + typeName.substring(typeName.lastIndexOf(".") + 1).toLowerCase() + "$" + munge(fqn);

                        ConstantRef ref = constants.get(constantName);
                        if (ref == null) {
                            ref = new ConstantRef(constantName, fqn, n);
                            constants.put(constantName, ref);
                        }
                        ref.setModule(t.getModule());

                        parent.replaceChild(n, IR.name(constantName));
                        break;

                    default:
                        break;
                }
            }
        }
    }
}
