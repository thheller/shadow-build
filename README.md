# shadow-build

A ClojureScript build library capable of compiling multiple output files. It is basically a rewrite of cljs.closure since it assumes to only ever produce one output file.

I need to come up with a good example why you'd want multiple output files.

Its a lot more flexible than cljs.closure which also means its a little more complex to use.

## Usage

Right now this really only is a library and not a tool like lein-cljsbuild. But Leiningen provides enough hooks to make integration easy, assuming you are using Leiningen of course.

### Step 1: Include the necessary options in your project.clj

```clojure
:profiles {:dev {:source-paths ["dev"]
                 :dependencies [[org.clojure/clojurescript "0.0-2277"]
                                [thheller/shadow-build "0.7.0"]]}}
```

I recommend putting it into the :dev profile since you usually don't need it in production. You'll also need to provide ClojureScript itself. Currently the minimum required version is 0.0-2127 (avoid releases 2197-2263).

As of 0.8.0 clojure.tools.logging is used for logging during compilation, see the sample dev/log4j.properties or use whatever logging framework you prefer. 

### Step 2: Create a build script.

See: https://github.com/thheller/shadow-build/blob/master/dev/build.clj

Needs more docs ....

### Step 3: Run it using Leiningen

```lein run -m build/dev```
or
```lein run -m build/production```

Create as many different builds as you like, they are just a function call away. Its also possible to compile multiple independent outputs in one go, see ```build/separate```

## License

Copyright © 2013 Thomas Heller

Distributed under the Eclipse Public License, the same as Clojure.
