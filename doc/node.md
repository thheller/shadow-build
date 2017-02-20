## cljs.nodejs: The Problem

The default node.js bundling strategy by CLJS works fine but has some drawbacks I wanted to address.

Also `(set! *main-cli-fn* foo)` and `(aset js/exports "foo" foo)` (or similar ways of injecting `exports`) need to die.

Also working around [issues](https://github.com/clojure/clojurescript/blob/772e59a3611d5c48dadb17587a1dd74a003751d0/src/main/cljs/cljs/bootstrap_node.js#L60-L92) (`require`, `__dirname`, `__filename` are not globals) instead of fixing the root issue needs to stop.

### `:none` vs optimized

`:none` will produce multiple files, while optimized code produces just one file.

- [bootstrap_node.js](https://github.com/clojure/clojurescript/blob/772e59a3611d5c48dadb17587a1dd74a003751d0/src/main/cljs/cljs/bootstrap_node.js) mechanism for loading files produces globals for `:none` but not when optimized.
- `js/__filename` and `js/__dirname` will be different (although you should never be using these)
- `(js/require "./something/relative.js")` will yield different things as it is either relative to the current `ns` file or the optimized file
- have to mess with `js/require.cache` to support live reloading
- `:none` will also pollute the output directory with many files

So, my goals were:

- provide a simple way to configure the entry points (`main-fn`) for scripts that is not done in code.
- provide a simple way to configure and produce UMD-compliant JS modules with `exports` (also not done in code).
- always generate one file (specified by `{:output-to "out/file.js"}`). Do not generate anything else in the `out` directory
- ensure `js/__filename`, `js/__dirname`, `js/require` behave identically for `:none` or higher
- produce no globals
- support hot loading + REPL


## My Solution

Node scripts:
```
(node/configure <compiler-state> {:main demo.foo/fancy-fn :output-to "out/foo.js"})
```

This will call `:main` when you run `node out/foo.js`, `*main-cli-fn*` is not involved.

UMD-compliant libs:
```
// compile config
(umd/configure <compiler-state>
  {:hello demo.foo/hello-fn}
  {:output-to "out/foo-lib.js"})
  
// in node
var lib = require("./out/foo-lib");
lib.hello();
```

The first map defines exports, the target function can be a normal `defn` it does not need to be `:export`ed.

### Implementation for :none

See: https://github.com/thheller/shadow-build/blob/master/src/clj/shadow/cljs/node_bootstrap.txt

The generated `:output-to` file uses a different bootstrap strategy from CLJS (or ClojureJS).

It first creates a `SHADOW_ENV` object instead of using `global`. `SHADOW_ENV` is only exposed in `:none` and should thus only be used by development related things.

The `SHADOW_IMPORT(relPath)` function will first read the file at `SHADOW_IMPORT_PATH` + `relPath`.

From the code every `goog.provide` will be extracted to find "root" names that must exist (ie. `cljs.core` must have a `cljs` object to work);

These roots will be created on the `SHADOW_ENV` object.

The code will then be wrapped similar to what `js/require` does by default. The wrapper will pull all "roots" into the local scope from the `SHADOW_ENV`. This is done because node will only look up vars in the local scope and `global` but not `SHADOW_ENV`.

```
(function (require, module, __filename, __dirname, SHADOW_ENV) {
var goog = SHADOW_ENV.goog;
var cljs = SHADOW_ENV.cljs;
var foo = SHADOW_ENV.foo;

// the actual code here
});
```

This code is then executed via `vm.runInThisContext` where the context is inside the `:output-to` file.

After creating the `SHADOW_IMPORT` function the generated file will start by importing `goog/base.js`.

`goog.require` is replace to do nothing as we control the import process and don't need the `goog` dependency management.

`goog.provide` is replaced to ensure objects are created on `SHADOW_ENV` and not `goog.global`.

Then all dependencies (ordered) of `:main` (or the exports) are imported. 

Then `:main` is called or the `exports` are created.

### Implementation for :whitespace or higher

Basically just pass everything into the Closure Compiler, then append the `:main` call or wrap it for UMD. Since there is now only one file there is no support for importing anything else.



### node.js platform internals

So I don't forget.

`js/require` works by reading the file and wrapping it in

```
NativeModule.wrapper = [
  '(function (exports, require, module, __filename, __dirname) { ',
  '\n});'
];
```

Every require'd file has its own scope. It is all executed via the `vm` package and `runInThisContext`.

`require.cache` is used to prevent loading a file multiple times. It uses the `require(id)` so if you attempt to load the same file via different `id` it will load it multiple times.





