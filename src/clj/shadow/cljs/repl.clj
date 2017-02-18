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
            [clojure.java.io :as io]
            [shadow.cljs.log :as log])
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
  (let [runtime-setup
        (cljs/make-runtime-setup state)

        cljs-user
        {:type :cljs
         :ns 'cljs.user
         :name "cljs/user.cljs"
         :js-name "cljs/user.js"
         :input (atom
                  (str "(ns cljs.user"
                       "(:require [cljs.repl :refer (doc find-doc source apropos pst dir)]))"))
         :provides #{'cljs.user}
         :requires #{'cljs.core 'runtime-setup 'cljs.repl}
         :require-order '[cljs.core runtime-setup cljs.repl]
         :last-modified (System/currentTimeMillis)}

        state
        (-> state
            (cljs/merge-resource runtime-setup)
            (cljs/merge-resource cljs-user))

        repl-state
        {:current {:ns 'cljs.user
                   :name "cljs/user.cljs"
                   ;; will be populated after compile
                   :ns-info nil}
         ;; the sources required to get the repl started
         :repl-sources []
         ;; each input and the action it should execute
         ;; keeps the entire history of the repl
         :repl-actions []
         }

        repl-sources
        (cljs/get-deps-for-ns state 'cljs.user)

        state
        (-> state
            (cljs/finalize-config)
            (cljs/do-compile-sources repl-sources))

        ns-info
        (get-in state [:compiler-env ::ana/namespaces 'cljs.user])

        repl-state
        (-> repl-state
            (assoc :repl-sources repl-sources
                   :repl-js-sources (->> repl-sources
                                         (map #(get-in state [:sources % :js-name]))
                                         (into [])))
            (assoc-in [:current :ns-info] ns-info))]

    (-> state
        (assoc :repl-state repl-state)
        (cljs/flush-sources-by-name repl-sources))
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
  ([state read-result quoted-require]
   (repl-require state read-result quoted-require nil))
  ([{:keys [repl-state] :as state}
    read-result
    quoted-require
    reload-flag]
    ;; FIXME: verify quoted
   (let [current-ns (get-in repl-state [:current :ns])
         require (remove-quotes quoted-require)
         ;; parsing this twice to easily get a diff, could probably be simpler
         {:keys [requires]} (util/parse-ns-require-parts :requires {} [require])
         new-requires (into #{} (vals requires))
         ;; returns the updated ns-info
         ns-info (util/parse-ns-require-parts :requires (get-in repl-state [:current :ns-info]) [require])

         deps (cljs/get-deps-for-entries state new-requires)
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
         (cljs/do-compile-sources deps)
         (cljs/flush-sources-by-name deps)
         (load-macros-and-set-ns-info)
         (update-in [:repl-state :repl-actions] conj
           {:type :repl/require
            :sources new-deps
            :js-sources (->> new-deps
                             (map #(get-in state [:sources % :js-name]))
                             (into []))
            :reload reload-flag})))))

(defn repl-load-file [{:keys [source-paths] :as state} read-result file-path]
  ;; FIXME: could clojure.core/load-file .clj files?

  (let [matched-paths
        (->> source-paths
             (vals)
             (filter :file)
             (filter
               (fn [{:keys [path] :as src-path}]
                 ;; without the / it will create 2 matches for
                 ;; something/src/clj
                 ;; something/src/cljs
                 (.startsWith file-path (str path "/"))))
             (into []))]

    (if (not= 1 (count matched-paths))
      ;; FIXME: configure it?
      (do (prn [:not-on-registered-source-path file-path matched-paths])
          state)

      ;; on registered source path
      ;; FIXME: could just reload if it exists? might be a recently created file, this covers both cases
      (let [{:keys [path] :as the-path}
            (first matched-paths)

            rc-name
            (subs file-path (-> path (count) (inc)))

            rc
            (cljs/make-fs-resource state path rc-name (io/file file-path))

            state
            (cljs/merge-resource state rc)

            deps
            (cljs/get-deps-for-src state rc-name)

            repl-deps
            (remove-already-required-repl-deps state deps)

            action
            {:type :repl/require
             :source repl-deps
             :js-sources
             (->> repl-deps
                  (map #(get-in state [:sources % :js-name]))
                  (into []))
             :reload :reload}]

        (-> state
            (cljs/do-compile-sources deps)
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
     [state read-result [q ns :as quoted-ns]]
     ;; quoted-ns is (quote the-ns)
     (if (nil? (get-in state [:provide->source ns]))
       ;; FIXME: create empty ns and switch to it
       (do (prn [:did-not-find ns])
           state)
       (let [{:keys [name ns-info]}
             (cljs/get-resource-for-provide state ns)

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

   'repl-state
   (fn [state read-result]
     (pprint (:repl-state state))
     state)

   'ns
   (fn [state read-result & args]
     (prn [:ns-not-yet-supported args])
     state)})

(defmethod log/event->str ::special-fn-error
  [{:keys [source special-fn error]}]
  (str special-fn " failed. " (str error)))

(defn process-read-result
  [{:keys [repl-state] :as state}
   {:keys [form source] :as read-result}]

  (cond
    ;; ('special-fn ...)
    ;; (require 'something)
    (and (list? form)
         (contains? repl-special-forms (first form)))
    (let [[special-fn & args]
          form

          handler
          (get repl-special-forms special-fn)]

      (try
        (apply handler state read-result args)
        (catch Exception e
          (cljs/log state {:type ::special-fn-error
                           :source source
                           :special-fn special-fn
                           :error e})
          state
          )))

    ;; compile normally
    :else
    (-> (cljs/with-compiler-env state
          (let [repl-action
                ;; FIXME: what actually populates this? emit or analyze?
                (cljs/with-warnings state
                  (binding [comp/*source-map-data*
                            (atom {:source-map (sorted-map)
                                   :gen-col 0
                                   :gen-line 0})]

                    {:type :repl/invoke
                     :js (let [ast (cljs/analyze state (:current repl-state) form :expr)]
                           (with-out-str
                             (comp/emit ast)))
                     :source source
                     ;; FIXME: need actual json source map
                     ;; :source-map (:source-map @comp/*source-map-data*)
                     }))]
            (update-in state [:repl-state :repl-actions] conj repl-action)
            )))))

(defn- read-one
  ([repl-state reader]
    (read-one repl-state reader {}))
  ([repl-state
    reader
    {:keys [filename] :or {filename "repl-input.cljs"}}]
   (let [eof-sentinel
         (Object.)

         opts
         {:eof eof-sentinel
          :read-cond :allow
          :features #{:cljs}}

         buf-len 1

         in
         (readers/source-logging-push-back-reader
           (PushbackReader. reader (object-array buf-len) buf-len buf-len)
           1
           filename)

         {:keys [ns ns-info] :as repl-rc}
         (:current repl-state)

         form
         (binding [*ns*
                   (create-ns ns)

                   ana/*cljs-ns*
                   ns

                   ana/*cljs-file*
                   name

                   reader/*data-readers*
                   tags/*cljs-data-readers*

                   reader/*alias-map*
                   (merge reader/*alias-map*
                     (:requires ns-info)
                     (:require-macros ns-info))]

           (readers/log-source in
             (reader/read opts in)))

         eof?
         (identical? form eof-sentinel)]

     (-> {:eof? eof?}
         (cond->
           (not eof?)
           (assoc :form form
                  :source
                  ;; FIXME: poking at the internals of SourceLoggingPushbackReader
                  ;; not using (-> form meta :source) which log-source provides
                  ;; since there are things that do not support IMeta, still want the source though
                  (-> @(.-source-log-frames in)
                      (:buffer)
                      (str)))))
     )))

(defn process-input
  "processes a string of forms, may read multiple forms"
  [state ^String repl-input]
  (let [reader
        (readers/string-reader repl-input)]

    (loop [{:keys [repl-state] :as state} state]

      (let [{:keys [eof?] :as read-result}
            (read-one repl-state reader)]

        (if eof?
          state
          (recur (process-read-result state read-result))))
      )))

(defn read-stream!
  "performs a blocking read given the current repl-state"
  [repl-state input-stream]
  (let [reader (readers/input-stream-reader input-stream)]
    (read-one repl-state reader)
    ))

(defn process-input-stream
  "reads one form of the input stream and calls process-form"
  [{:keys [repl-state] :as state} input-stream]
  (let [{:keys [eof?] :as read-result}
        (read-stream! repl-state input-stream)]
    (if eof?
      state
      (process-read-result state read-result))))

