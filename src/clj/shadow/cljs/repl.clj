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
            [clojure.repl :as repl]
            [shadow.cljs.util :as util]
            [cljs.env :as env]
            [clojure.java.io :as io])
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
                                          :input (atom
                                                   (str "(ns cljs.user"
                                                        "(:require [cljs.repl :refer (doc find-doc source apropos pst dir)]))"))
                                          :provides #{'cljs.user}
                                          :requires #{'cljs.core 'runtime-setup 'cljs.repl}})

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
                       (assoc :repl-sources repl-sources
                              :repl-js-sources (->> repl-sources
                                                    (map #(get-in state [:sources % :js-name]))
                                                    (into [])))
                       (assoc-in [:current :ns-info] ns-info))

        state (assoc state :repl-state repl-state)]

    (cljs/flush-sources-by-name state repl-sources)
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

(defn- remove-already-required-repl-deps
  ;; FIXME: currently only removes deps required on repl init not those required by individual actions
  [{:keys [repl-state] :as state} deps]
  (let [old-deps (into #{} (:repl-sources repl-state))]
    (->> deps (remove old-deps) (into []))))

(defn repl-require
  ([state source quoted-require]
   (repl-require state source quoted-require nil))
  ([{:keys [repl-state] :as state} source quoted-require reload-flag]
    ;; FIXME: verify quoted
   (let [current-ns (get-in repl-state [:current :ns])
         require (remove-quotes quoted-require)
         ;; parsing this twice to easily get a diff, could probably be simpler
         {:keys [requires]} (util/parse-ns-require-parts :requires {} [require])
         new-requires (into #{} (vals requires))
         ;; returns the updated ns-info
         ns-info (util/parse-ns-require-parts :requires (get-in repl-state [:current :ns-info]) [require])

         deps (cljs/get-deps-for-mains state new-requires)
         new-deps (remove-already-required-repl-deps state deps)

         load-macros-and-set-ns-info
         (fn [state]
           (cljs/with-compiler-env state
             (let [full-ns-info
                   (-> ns-info
                       ;; FIXME: these work with env/*compiler* but shouldn't
                       (util/load-macros)
                       (util/infer-macro-require)
                       (util/infer-macro-use))]

               ;; FIXME: util/check-uses!
               (-> state
                   (cljs/swap-compiler-env! update-in [::ana/namespaces current-ns] merge full-ns-info)
                   (assoc-in [:repl-state :current :ns-info] full-ns-info))
               )))]

     (-> state
         (cljs/compile-sources deps)
         (cljs/flush-sources-by-name deps)
         (load-macros-and-set-ns-info)
         (update-in [:repl-state :repl-actions] conj {:type :repl/require
                                                      :sources new-deps
                                                      :js-sources (->> new-deps
                                                                       (map #(get-in state [:sources % :js-name]))
                                                                       (into []))
                                                      :reload reload-flag
                                                      :source source})))))

(defn repl-load-file [{:keys [source-paths] :as state} source file-path]
  (let [registered-src-paths
        (->> source-paths
             (vals)
             (filter :abs-path)
             (filter #(.startsWith file-path (:abs-path %)))
             (into []))]
    (if (not= 1 (count registered-src-paths))
      ;; FIXME: configure it?
      (do (prn [:not-on-registered-source-path file-path])
          state)

      ;; on registered source path
      ;; FIXME: could just reload if it exists? might be a recently created file, this covers both cases
      (let [{:keys [abs-path path] :as the-path} (first registered-src-paths)
            rc-name (subs file-path (-> abs-path (count) (inc)))
            rc (cljs/make-fs-resource state path rc-name (io/file file-path))
            state (cljs/merge-resource state rc)
            deps (cljs/get-deps-for-src state rc-name)
            repl-deps (remove-already-required-repl-deps state deps)
            action {:type :repl/require
                    :source repl-deps
                    :js-sources (->> repl-deps
                                     (map #(get-in state [:sources % :js-name]))
                                     (into []))
                    :reload :reload}]

        (-> state
            (cljs/compile-sources deps)
            (cljs/flush-sources-by-name deps)
            (update-in [:repl-state :repl-actions] conj action))
        ))))

(def repl-special-forms
  {'require
   repl-require

   'cljs.core/require
   repl-require

   'load-file
   repl-load-file

   'cljs.core/load-file
   repl-load-file

   'in-ns
   (fn repl-in-ns
     [state source [q ns :as quoted-ns]]
     ;; quoted-ns is (quote the-ns)
     (if (nil? (get-in state [:provide->source ns]))
       ;; FIXME: create empty ns and switch to it
       (do (prn [:did-not-find ns])
           state)
       (let [{:keys [name ns-info]} (cljs/get-resource-for-provide state ns)
             set-ns-action
             {:type :repl/set-ns
              :ns ns
              :name name}]
         (-> state
             ;; FIXME: clojure in-ns doesn't actually do the ns setup
             ;; so we should merge an ns-info only if ns is already loaded
             ;; otherwise keep it empty
             (update-in [:repl-state :current] merge {:ns ns
                                                      :name name
                                                      :ns-info ns-info})
             (update-in [:repl-state :repl-actions] conj set-ns-action)
             ))))

   'repl-dump
   (fn [state source]
     (pprint (:repl-state state))
     state)

   'ns
   (fn [state source & args]
     (prn [:ns-not-yet-supported source])
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
        opts {:eof eof-sentinel
              :read-cond :allow :features #{:cljs}}

        reader (DerefStringReader. repl-input (count repl-input) 0)
        buf-len 1
        ;; https://github.com/clojure/tools.reader/blob/master/src/main/clojure/clojure/tools/reader/reader_types.clj#L271
        in (readers/indexing-push-back-reader (PushbackReader. reader (object-array buf-len) buf-len buf-len) 1 "repl-input.cljs")]

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
          ;; eof
          (identical? form eof-sentinel)
          state

          ;; ('special-fn ...)
          ;; (require 'something)
          (and (list? form)
               (contains? repl-special-forms (first form)))
          (let [[special-fn & args] form
                handler (get repl-special-forms special-fn)]
            (recur (try
                     (apply handler state source args)
                     (catch Exception e
                       (prn [:special-fn-error source])
                       (repl/pst e)
                       state
                       ))))

          ;; compile normally
          :else
          (-> (cljs/with-compiler-env state
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
                                     ast (if (= :var (:op ast))
                                           (assoc-in ast [:env :context] :expr)
                                           ast)]

                                 (with-out-str
                                   (comp/emit ast)))
                           ;; FIXME: need actual json source map
                           ;; :source-map (:source-map @comp/*source-map-data*)
                           }))]
                  (update-in state [:repl-state :repl-actions] conj repl-action)
                  ))
              (recur))
          )))))

