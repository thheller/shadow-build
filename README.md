# shadow-build

A ClojureScript build library capable of compiling multiple output files. It is basically a rewrite of cljs.closure since it assumes to only ever produce one output file.

Its a lot more flexible than cljs.closure which also means its a little more complex to use.

Example Project can be found at: http://github.com/thheller/shadow-build-example

## Usage

Right now this really only is a library and not a tool like lein-cljsbuild. But Leiningen provides enough hooks to make integration easy, assuming you are using Leiningen of course.

### Include the necessary options in your project.clj

```clojure
:profiles {:dev {:source-paths ["dev"]
                 :dependencies [[org.clojure/clojurescript "1.7.48"]
                                [thheller/shadow-build "1.0.0-alpha15"]]}}
```

alpha11+ also require Clojure 1.7.0-RC1 and up!

I recommend putting it into the :dev profile since you usually don't need it in production. You'll also need to provide ClojureScript itself.

### Simple CLJS Build

See: https://github.com/thheller/shadow-build-example

shadow-build includes 2 common build configurations and we use leiningen to configure/execute them.

- ```cljs-dev```: development mode, includes source maps and auto compiles cljs on file changes
- ```cljs-prod```: production level build, advanced compilation

Using these requires the following additions to your project.clj

```clojure
:aliases {"cljs-dev" ["run" "-m" "shadow.cljs.api/build-dev" :project/cljs]
          "cljs-prod" ["run" "-m" "shadow.cljs.api/build-prod" :project/cljs]}
  
:cljs {:modules [{:name :cljs
                  :main 'my-project.app}]

       :source-paths ["src/cljs"]
       :public-dir "public/assets/js"
       :public-path "/assets/js"}
```

The ```:aliases``` feature of leiningen will simply call the ```shadow.cljs.api/build-(dev|prod)``` functions with the ```:cljs``` map as argument. The build config (```:cljs```) requires very little information:

- ```:source-paths``` where is your cljs
- ```:public-dir``` the directory where the output should go
- ```:public-path``` the path where :public-dir is reachable by your webserver (should be absolute)
- ```:modules``` a list of modules your build should be split into (optional, if you just define one)

A module requires a ```:name``` value which should be a keyword, it will produce a .js file by that name in ```:public-dir``` after compilation. It must define at least one ```:main``` which is either a symbol or collection of symbols refering to the entry points to your application, usually one ```(ns ...)``` which contains exported functions. Additionally if you define multiple modules their dependencies must be declared via ```:depends-on #{:mod-1 :mod-2}```.

When configured you can run these via ```lein cljs-dev```. You should end up with a ```public/assets/js/cljs.js``` which you can just include in your HTML.

### Advanced Configuration: Create a build script.

See: https://github.com/thheller/shadow-build/blob/master/dev/build.clj

Needs more docs, but you can execute each build function you define via lein, either with an alias or directly on the commmand line ```lein run -m build/dev```

## License

Copyright © 2015 Thomas Heller

Distributed under the Eclipse Public License, the same as Clojure.
