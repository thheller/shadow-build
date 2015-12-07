# shadow-build

A ClojureScript build library capable of compiling multiple output files. It is basically a rewrite of cljs.closure since it assumes to only ever produce one output file.

Its a lot more flexible than cljs.closure which also means its a little more complex to use.

Example Project can be found at: http://github.com/thheller/shadow-build-example

## Usage

Right now this really only is a library and not a tool like lein-cljsbuild. But Leiningen provides enough hooks to make integration easy, assuming you are using Leiningen of course.

### Include the necessary options in your project.clj

```clojure
:profiles {:dev {:source-paths ["dev"]
                 :dependencies [[org.clojure/clojurescript "1.7.170"]
                                [thheller/shadow-build "1.0.188"]]}}
```

alpha11+ also require Clojure 1.7.0-RC1 and up!

I recommend putting it into the :dev profile since you usually don't need it in production. You'll also need to provide ClojureScript itself.

For a basic usage example see: http://github.com/thheller/shadow-build-example

## License

Copyright © 2015 Thomas Heller

Distributed under the Eclipse Public License, the same as Clojure.
