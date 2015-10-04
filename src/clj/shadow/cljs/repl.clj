(ns shadow.cljs.repl
  (:require [shadow.cljs.build :as cljs]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [clojure.tools.reader :as reader]
            [cljs.tagged-literals :as tags]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [shadow.cljs.util :as util])
  (:import (clojure.tools.reader.reader_types PushbackReader StringReader)))

(comment
  (def repl-state
    {:current cljs-resource
     :repl-sources list-of-source-names-required-on-repl-init
     :repl-actions list-of-repl-actions-and-the-input-that-created-them}))

(defn prepare [state]
  ;; FIXME: less hardcoded cljs.user

  ;; must compile an empty cljs.user to properly populate the ::ana/namespaces
  ;; could just manually set the values needed but I don't want to keep track what gets set
  ;; so just pretend there is actually an empty ns we never user
  (let [state (cljs/merge-resource state (cljs/make-runtime-setup state))
        state (cljs/merge-resource state {:type :cljs
                                          :ns 'cljs.user
                                          :name "cljs/user.cljs"
                                          :js-name "cljs/user.js"
                                          :input (atom "(ns cljs.user)")
                                          :provides #{'cljs.user}
                                          :requires #{'cljs.core 'runtime-setup}})

        repl-state {:current {:ns 'cljs.user
                              :name "cljs/user.cljs"
                              ;; will be populated after compile
                              :ns-info nil}
                    ;; the sources required to get the repl started
                    :repl-sources []
                    ;; each input and the action it should execute
                    ;; keeps the entire history of the repl
                    :repl-actions []
                    }

        repl-sources (cljs/get-deps-for-ns state 'cljs.user)
        state (cljs/compile-sources state repl-sources)

        ns-info (get-in state [:compiler-env ::ana/namespaces 'cljs.user])

        repl-state (-> repl-state
                       (assoc :repl-sources repl-sources)
                       (assoc-in [:current :ns-info] ns-info))

        state (assoc state :repl-state repl-state)]

    state
    ))


(defn remove-quotes [quoted-form]
  (walk/prewalk
    (fn [form]
      (if (and (list? form)
               (= 'quote (first form)))
        (second form)
        form
        ))
    quoted-form))


(def repl-special-forms
  {'require
   (fn [{:keys [repl-state public-path] :as state} [quoted-require reload-flag] source]
     (let [require (remove-quotes quoted-require)
           ;; parsing this twice to easily get a diff, could probably be simpler
           {:keys [requires]} (util/parse-ns-require-parts :requires {} [require])
           new-requires (into #{} (vals requires))
           ;; returns the updated ns-info
           ns-info (util/parse-ns-require-parts :requires (get-in repl-state [:current :ns-info]) [require])
           deps (cljs/get-deps-for-mains state new-requires)
           old-deps (into #{} (:repl-sources repl-state))
           new-deps (->> deps (remove old-deps) (into []))]

       (-> state
           ;; FIXME: should assoc ns-info in :sources also so we can get it back later
           ;; FIXME: also needs to flush
           (cljs/compile-sources deps)
           (assoc-in [:repl-state :current :ns-info] ns-info)
           (update-in [:repl-state :repl-actions] conj {:type :repl/require
                                                        :sources new-deps
                                                        :source-paths (mapv #(str public-path "/" %) new-deps)
                                                        :reload reload-flag
                                                        :source source}))))

   'repl-dump
   (fn [state args source]
     (pprint (:repl-state state))
     state)

   'load-file
   (fn [state [file-path] source]
     (prn [:load-file file-path])
     state)

   'ns
   (fn [state args source]
     state)})

;; https://github.com/clojure/tools.reader/blob/master/src/main/clojure/clojure/tools/reader/reader_types.clj#L47
;; only addition is that we can get at s-pos
;; tools.reader has source logging but I couldn't figure out if that does what I want
(deftype DerefStringReader
  [^String s s-len ^:unsynchronized-mutable s-pos]

  clojure.lang.IDeref
  (deref [_]
    s-pos)

  readers/Reader
  (read-char [_]
    (when (> s-len s-pos)
      (let [r (nth s s-pos)]
        (set! s-pos (inc s-pos))
        r)))
  (peek-char [_]
    (when (> s-len s-pos)
      (nth s s-pos))))

(defn process-input
  [{:keys [repl-state] :as state} repl-input]
  (let [eof-sentinel (Object.)
        opts (merge
               {:eof eof-sentinel}
               {:read-cond :allow :features #{:cljs}})

        reader (DerefStringReader. repl-input (count repl-input) 0)
        buf-len 1
        ;; https://github.com/clojure/tools.reader/blob/master/src/main/clojure/clojure/tools/reader/reader_types.clj#L271
        in (readers/indexing-push-back-reader (PushbackReader. reader (object-array buf-len) buf-len buf-len) 1 "repl-input.cljs")]

    (cljs/with-compiler-env state
      (loop [{:keys [repl-state] :as state} state]

        (let [{:keys [ns ns-info] :as repl-rc} (:current repl-state)
              form-start @reader
              form (binding [*ns* (create-ns ns)
                             ana/*cljs-ns* ns
                             ana/*cljs-file* name
                             reader/*data-readers* tags/*cljs-data-readers*
                             reader/*alias-map* (merge reader/*alias-map*
                                                       (:requires ns-info)
                                                       (:require-macros ns-info))]
                     (reader/read opts in))
              form-end @reader
              ;; keep a reference to the source of the form
              source (subs repl-input form-start form-end)]
          (cond
            (identical? form eof-sentinel)
            ;; eof
            state

            ;; ('special-fn ...)
            (and (list? form)
                 (contains? repl-special-forms (first form)))
            (let [[special-fn & args] form
                  handler (get repl-special-forms special-fn)]
              (recur (handler state args source)))

            ;; compile normally
            :else
            (let [repl-action
                  ;; FIXME: what actually populates this? emit or analyze?
                  (cljs/with-warnings
                    (binding [comp/*source-map-data* (atom {:source-map (sorted-map)
                                                            :gen-col 0
                                                            :gen-line 0})]
                      {:type :repl/invoke
                       :js (let [ast (cljs/analyze state repl-rc form)
                                 ;; cheat and turn everything into an expr
                                 ;; "(def x 1)"
                                 ;; "x"
                                 ;; since :context defaults to :statement and cljs.user.x is a useless :statement
                                 ;; it will not generate any code
                                 ;; with :expr it will correctly emit some code
                                 ;; has the side effect of removing ";\n" from actual statements but since each
                                 ;; snippet of code generated here is meant to directly eval(str) it doesn't matter
                                 ;; FIXME: check if that breaks something
                                 ;; FIXME: should probably to this to (ana/empty-env) before the actually analyze
                                 ast (assoc-in ast [:env :context] :expr)]

                             (with-out-str
                               (comp/emit ast)))
                       ;; FIXME: need actual json source map
                       ;; :source-map (:source-map @comp/*source-map-data*)
                       }))]
              (-> state
                  (update-in [:repl-state :repl-actions] conj repl-action)
                  (recur))
              )))))))

