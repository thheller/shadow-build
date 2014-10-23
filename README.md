# shadow-build

A ClojureScript build library capable of compiling multiple output files. It is basically a rewrite of cljs.closure since it assumes to only ever produce one output file.

Its a lot more flexible than cljs.closure which also means its a little more complex to use.

## Closure Modules

Modules allow the compiler to optimize the dependencies of your code for you. Usually (in jQuery-Land) you have to include all necessary javascript files (jquery.plugin-a.js, jquery.plugin-b.js, ...) on every page that needs it. This usually ends in loading all files all the time cause who knows whats needed and what not. With modules you'd group common functionality into a API namespace which serves as an entry point for the module. They are not like usual JavaScript modules though (RequireJS, AMD, ...), although they are similar except that they export names in their own which you cannot bind yourself.

If you have big chunks of code in your app that are only required in certain circumstances (eg. only when the user is logged in, only for the shop checkout process, only for admins, only on Tuesdays, ...) you'd create feature module "checkout" and a ```(ns my-app.checkout)``` main as its entry point. In your HTML include the checkout module and you are done. If the checkout now starts using fancy new library it will be managed on the ```(ns my-app.checkout (:require))``` side of things, no need to touch the HTML or build scripts.

## Usage

Right now this really only is a library and not a tool like lein-cljsbuild. But Leiningen provides enough hooks to make integration easy, assuming you are using Leiningen of course.

### Step 1: Include the necessary options in your project.clj

```clojure
:profiles {:dev {:source-paths ["dev"]
                 :dependencies [[org.clojure/clojurescript "0.0-2322"]
                                [thheller/shadow-build "0.9.5"]]}}
```

I recommend putting it into the :dev profile since you usually don't need it in production. You'll also need to provide ClojureScript itself. Currently the minimum required version is 0.0-2127 (avoid releases 2197-2263).

### Step 2: Create a build script.

See: https://github.com/thheller/shadow-build/blob/master/dev/build.clj

Needs more docs ....

### Step 3: Run it using Leiningen

```lein run -m build/dev```
or
```lein run -m build/production```

Create as many different builds as you like, they are just a function call away. Its also possible to compile multiple independent outputs in one go, see ```build/separate```

## License

Copyright © 2014 Thomas Heller

Distributed under the Eclipse Public License, the same as Clojure.
