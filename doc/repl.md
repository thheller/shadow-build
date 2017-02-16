# shadow.cljs.repl

I wrote this as an alternate implementation to the standard CLJS REPL. The default REPL is a somewhat closed system trying to hide the JS runtime. While you can hook into each part of the process (read, eval, print, prompt, etc.) it assumes that "it" is the "Driver". This is very much modelled after the standard CLJ REPL.

I wanted to make a distinction between CLJS "the compiler" and CLJS "the runtime".

Technically speaking `shadow.cljs.repl` is not a REPL at all, it is only an interface to the compiler. The minimal amount of state required to operate the REPL commands is kept in a special property and the actual REPL can drive this incrementally by processing chunks of CLJS code. The compiler will produce corresponding `:repl-actions`. Basically instead of a hard-coded loop we get a RPC system.

## Basic setup

Everything `shadow-build` starts out with the initial state and resource discovery. Configuration (like `enable-source-maps`) should be done before the first compilation happens. All steps take the current compiler state and return the new state. No other global state is created or needed. 

```
(require '[shadow.cljs.build :as cljs])
(require '[shadow.cljs.repl :as repl])
(require '[clojure.pprint :refer (pprint)])

(let [{:keys [repl-state] :as state}
      (-> (cljs/init-state)
          (cljs/enable-source-maps)
          (cljs/find-resources-in-classpath)
          (repl/prepare))]
  (pprint repl-state))
```

The `shadow.cljs.repl` namespaces operates on the `:repl-state` k/v pair inside the compiler state.

The basic structure is
```
{:current the-current-repl-ns
 :repl-sources [...]
 :repl-js-sources [...]
 :repl-actions [...]}
```

`:repl-js-sources` is a vector of strings representing the names of JS files the JS runtime needs to load. These are in dependency order and are required for the REPL to work.

The above call generates this:

```
{:current
 {:ns cljs.user,
  :name "cljs/user.cljs",
  :ns-info
  {:rename-macros {},
   :renames {},
   :meta nil,
   :require-order [cljs.repl],
   :use-macros
   {doc cljs.repl,
    find-doc cljs.repl,
    source cljs.repl,
    apropos cljs.repl,
    pst cljs.repl,
    dir cljs.repl},
   :excludes #{},
   :name cljs.user,
   :requires {cljs.repl cljs.repl},
   :seen #{:require},
   :uses
   {doc cljs.repl,
    find-doc cljs.repl,
    source cljs.repl,
    apropos cljs.repl,
    pst cljs.repl,
    dir cljs.repl},
   :require-macros {cljs.repl cljs.repl}}},
 :repl-sources
 ["goog/reflect/reflect.js"
  "goog/math/long.js"
  "goog/math/integer.js"
  "goog/string/string.js"
  "goog/object/object.js"
  "goog/debug/error.js"
  "goog/dom/nodetype.js"
  "goog/asserts/asserts.js"
  "goog/array/array.js"
  "goog/string/stringbuffer.js"
  "cljs/core.cljs"
  "runtime_setup.js"
  "clojure/walk.cljs"
  "cljs/spec/impl/gen.cljs"
  "clojure/string.cljs"
  "cljs/spec.cljs"
  "cljs/repl.cljs"
  "cljs/user.cljs"],
 :repl-actions [],
 :repl-js-sources
 ["goog/reflect/reflect.js"
  "goog/math/long.js"
  "goog/math/integer.js"
  "goog/string/string.js"
  "goog/object/object.js"
  "goog/debug/error.js"
  "goog/dom/nodetype.js"
  "goog/asserts/asserts.js"
  "goog/array/array.js"
  "goog/string/stringbuffer.js"
  "cljs/core.js"
  "runtime_setup.js"
  "clojure/walk.js"
  "cljs/spec/impl/gen.js"
  "clojure/string.js"
  "cljs/spec.js"
  "cljs/repl.js"
  "cljs/user.js"]}
```

The runtime should take this information and ensure that the `:repl-js-sources` are loaded before proceeding.

## `:repl-actions`

The compiler interface expects a full form to read, it does not operate on a stream. You feed input to the REPL via `repl/process-input`. Each input will only affect the `:repl-actions` in `:repl-state`. Each entry will have a `:type`.

### `:repl/invoke`

```
(let [{:keys [repl-state] :as state}
      (-> (cljs/init-state)
          (cljs/enable-source-maps)
          (cljs/find-resources-in-classpath)
          (repl/prepare)
          (repl/process-input "(prn :hello-world)")
          )]
  (pprint (:repl-actions repl-state)))
```

This example produced a `:repl/invoke` with a `:js` property for the compiled JS ready to eval. `:warnings` contains eventual warnings produced by the code so the runtime may decide to skip the eval.

```
[{:type :repl/invoke,
  :js
  "cljs.core.prn.call(null,new cljs.core.Keyword(null,\"hello-world\",\"hello-world\",-788431463))",
  :warnings []}]
```

Example with `:warnings` via `(repl/process-input "(prn foo)")`:

```
[{:type :repl/invoke,
  :js "cljs.core.prn.call(null,cljs.user.foo)",
  :warnings
  [{:warning :undeclared-var,
    :line 1,
    :column 6,
    :msg "Use of undeclared Var cljs.user/foo",
    :extra {:prefix cljs.user, :suffix foo, :macro-present? false}}]}]
```

Multiple `:repl-actions` will be generated should the argument to `process-input` contain multiple forms

```
(repl/process-input "(prn :foo) (prn :bar)")

[{:type :repl/invoke,
  :js
  "cljs.core.prn.call(null,new cljs.core.Keyword(null,\"foo\",\"foo\",1268894036))",
  :warnings []}
 {:type :repl/invoke,
  :js
  "cljs.core.prn.call(null,new cljs.core.Keyword(null,\"bar\",\"bar\",-1386246584))",
  :warnings []}]
```

### `:repl/set-ns`

Result of `(repl/process-input "(in-ns 'cljs.core)")`. It will also affect the `:current` or `:repl-state`.

```
[{:type :repl/set-ns, :ns cljs.core, :name "cljs/core.cljs"}]
```

### `:repl/require`

Result of `require` and other related forms: `(repl/process-input "(require 'goog.async.nextTick))`

```
[{:type :repl/require,
  :sources
  ["goog/debug/entrypointregistry.js"
   "goog/dom/tagname.js"
   "goog/functions/functions.js"
   "goog/labs/useragent/util.js"
   "goog/labs/useragent/browser.js"
   "goog/labs/useragent/engine.js"
   "goog/async/nexttick.js"],
  :js-sources
  ["goog/debug/entrypointregistry.js"
   "goog/dom/tagname.js"
   "goog/functions/functions.js"
   "goog/labs/useragent/util.js"
   "goog/labs/useragent/browser.js"
   "goog/labs/useragent/engine.js"
   "goog/async/nexttick.js"],
  :reload nil,
  :source "(require 'goog.async.nextTick)"}]
```

Note that this does not actually include any JS to eval. It only informs the runtime of which files it needs to load (in order). `:js-sources` is the list of JS files and `:sources` if their original source name (for `.cljs` files mostly). How these files are loaded is up to the runtime. Given the vastly different capabilities of JS runtimes it would be to hard to try to fit them all into the compiler (IMHO).

`load-file` is also a special form, it does not use `cljs.core/load-file`. It expects an absolute path and has the restriction that the file being loaded must be on a valid source path. `load-file` only works in the REPL, not the normal CLJS runtime.

I have not implemented the `ns` special form yet but it would probably emit a combination of `:repl/require` and `:repl/set-ns`. It would definitely not emit any `:repl/invoke` since there is nothing to eval.

## TBD: How about an actual REPL?

shadow-devtools has one, currently in the process of restructuring that though.

