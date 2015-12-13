(ns shadow.cljs.util
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [cljs.analyzer :as ana]
            [cljs.env :as env]
            [clojure.pprint :refer (pprint)]))

(def require-option-keys
  #{:as
    :refer
    :refer-macros
    :include-macros})

(def use-option-keys
  #{:only})

(def uses-key
  {:requires :uses
   :require-macros :use-macros})

(defn- check-require-once! [{:keys [name requires] :as ns-info} require-ns]
  (when (some #(= % require-ns) (vals requires))
    (throw (ex-info (format "NS:%s has duplicate require/use for %s" name require-ns) {:ns-info ns-info}))
    ))

(defn parse-ns-require-parts
  [key {:keys [form] :as ns-info} parts]
  (reduce
    (fn [ns-info part]
      (cond
        (or (= :requires key)
            (= :require-macros key))
        (cond
          ;; (:require foo) => {:require {foo foo}}
          (symbol? part)
          (-> ns-info
              (assoc-in [key part] part)
              (cond->
                (= key :requires)
                (update :require-order conj part)))

          (or (= :reload-all part)
              (= :reload part))
          (assoc ns-info part (set (vals (get ns-info key))))

          ;; (:require [foo :as bar :refer (baz)]) => {:require {foo bar} :use {baz foo}}
          (sequential? part)
          (let [[require-ns & more] part]
            (when-not (even? (count more))
              (throw (ex-info "Only [lib.ns & options] and lib.ns specs supported in :require / :require-macros" {:form form :part part})))

            (let [options (apply hash-map more)]
              ;; FIXME: check that each key only appears once, (:require [some :as x :as y]) should not be valid
              (when-not (set/subset? (keys options) require-option-keys)
                (throw (ex-info (str "Only :as alias and :refer (names) options supported in " key) {:form form :part part})))

              (when (= :requires key)
                (check-require-once! ns-info require-ns))

              (let [alias (:as options)

                    ns-info (assoc-in ns-info [key require-ns] require-ns)

                    ;; :require-macros should not be in require-order since it won't have a js file to load
                    ns-info (if (= :requires key)
                              (update ns-info :require-order conj require-ns)
                              ns-info)

                    ns-info (if alias
                              (assoc-in ns-info [key alias] require-ns)
                              ns-info)

                    ns-info (if-let [refer (get options :refer)]
                              (do (when-not (sequential? refer)
                                    (throw (ex-info ":refer (names) must be sequential" {:form form :part part})))
                                  (reduce
                                    (fn [ns-info refer]
                                      (assoc-in ns-info [(get uses-key key) refer] require-ns))
                                    ns-info
                                    refer))
                              ns-info)

                    ns-info (let [refer-macros (:refer-macros options)
                                  merge-refer-macros (fn [ns-info]
                                                       (reduce
                                                         (fn [ns-info use]
                                                           (assoc-in ns-info [:use-macros use] require-ns))
                                                         ns-info
                                                         refer-macros))]
                              (if (or (:include-macros options) (sequential? refer-macros))
                                (-> ns-info
                                    (assoc-in [:require-macros require-ns] require-ns)
                                    (merge-refer-macros)
                                    (cond->
                                      alias
                                      (assoc-in [:require-macros alias] require-ns)))
                                ns-info))
                    ]
                ns-info)))

          :else
          (throw (ex-info "Unsupported part in form" {:form form :part part :key key})))

        ;; (:use [foo :only (baz)]) => {:uses {baz foo}}
        (or (= :uses key)
            (= :use-macros key))
        (do (when-not (sequential? part)
              (throw (ex-info "Only [lib.ns :only (names)] specs supported in :use / :use-macros" {:form form :part part})))


            (let [[use-ns & more] part]
              (when-not (even? (count more))
                (throw (ex-info "Only [lib.ns & options] and lib.ns specs supported in :use / :use-macros" {:form form :part part})))


              (let [{:keys [only] :as options} (apply hash-map more)]
                (when (not (and (= 1 (count options))
                                (sequential? only)))
                  (throw (ex-info (str "Only :only (names) options supported in " key) {:form form :part part})))

                (when (= :uses key)
                  (check-require-once! ns-info use-ns))

                (let [ns-info (if (= :uses key)
                                (-> ns-info
                                    (assoc-in [:requires use-ns] use-ns)
                                    (update :require-order conj use-ns))
                                (assoc-in ns-info [:require-macros use-ns] use-ns))
                      ns-info (reduce
                                (fn [ns-info use]
                                  (assoc-in ns-info [key use] use-ns))
                                ns-info
                                only)]
                  ns-info
                  ))))

        :else
        (throw (ex-info "how did you get here?" {:ns-info ns-info :key key :part part}))
        ))

    ns-info
    parts))

(defn parse-ns-refer-clojure
  [ns-info args]
  (when-not (even? (count args))
    (throw (ex-info "Only (:refer-clojure :exclude (foo bar)) allowed" {})))
  (let [{:keys [exclude] :as options} (apply hash-map args)]
    (update-in ns-info [:excludes] into exclude)
    ))

(defn parse-ns-import
  [ns-info parts]
  (reduce
    (fn [ns-info part]

      (cond
        (symbol? part)
        (let [class (-> part str (str/split #"\.") last symbol)]
          (-> ns-info
              (assoc-in [:imports class] part)
              (assoc-in [:requires class] part)
              (update :require-order conj part)))

        (sequential? part)
        (let [[ns & classes] part]
          (when-not (and (symbol? ns)
                         (every? symbol? classes))
            (throw (ex-info "[lib.ns Ctor*] violation" {:part part})))

          (reduce
            (fn [ns-info class]
              (let [fqn (symbol (str ns "." class))]
                (-> ns-info
                    (assoc-in [:imports class] fqn)
                    (assoc-in [:requires class] fqn)
                    (update :require-order conj fqn))))
            ns-info
            classes))))
    ns-info
    parts))

(defn parse-ns
  "expected a parse ns form from the reader, returns a map with the extracted information"
  [[head ns-name & more :as form]]
  (when-not (= 'ns head)
    (throw (ex-info "Not an (ns ...) form" {:form form})))
  (when-not (symbol? ns-name)
    (throw (ex-info "Namespaces must be named by a symbol." {:form form})))

  (let [first-arg (first more)
        [meta more] (cond
                      (and (string? first-arg) (map? (second more)))
                      [(assoc (second more) :doc first-arg) (drop 2 more)]
                      (string? first-arg)
                      [{:doc first-arg} (rest more)]
                      (map? first-arg)
                      [first-arg (rest more)]
                      :else
                      [nil more])
        ns-info
        (reduce
          (fn [{:keys [seen] :as ns-info} part]
            (when-not (sequential? part)
              (throw (ex-info "unrecognized ns part" {:form form :part part})))

            (let [[head & tail] part]
              (when (contains? seen head)
                (throw (ex-info (str "Only one " head " allowed") {:form form :part part})))
              (-> (cond
                    (= :require head)
                    (parse-ns-require-parts :requires ns-info tail)

                    (= :require-macros head)
                    (parse-ns-require-parts :require-macros ns-info tail)

                    (= :use head)
                    (parse-ns-require-parts :uses ns-info tail)

                    (= :use-macros head)
                    (parse-ns-require-parts :use-macros ns-info tail)

                    (= :import head)
                    (parse-ns-import ns-info tail)

                    (= :refer-clojure head)
                    (parse-ns-refer-clojure ns-info tail)

                    :else
                    (throw (ex-info "Unsupport part in ns form" {:form form :part part})))

                  (update-in [:seen] conj head))))

          {:excludes #{}
           :seen #{}
           :name (vary-meta ns-name merge meta)
           :requires {}
           :require-order []
           :require-macros {}
           :uses {}
           :use-macros {}}
          more)]

    (when (not= (count (:require-order ns-info))
                (count (->> (:requires ns-info)
                            (vals)
                            (into #{}))))
      ;; require-order should always match all required cljs namespaces
      ;; but since :requires is a map that contains {alias full-name, full-name full-name}
      ;; convert it to a set first, this also checks if require-order contains duplicates
      ;; since the counts wont match
      ;; FIXME: sanity check this properly, add better error since any error is a bug in this code
      (throw (ex-info "messed up requires" {:ns-info ns-info})))

    ns-info))


(defn find-macros-in-ns
  [name]
  (->> (ns-publics name)
       (reduce-kv (fn [m var-name the-var]
                    (if (.isMacro ^clojure.lang.Var the-var)
                      (conj m var-name)
                      m))
         #{})))


(def ^{:private true} require-lock (Object.))

(defn load-macros
  [{:keys [name require-macros use-macros] :as ast}]
  (if (= 'cljs.core name)
    ast
    (let [macro-namespaces
          (-> #{}
              (into (vals require-macros))
              (into (vals use-macros)))]

      (binding [ana/*cljs-ns* name]
        (locking require-lock
          (doseq [macro-ns macro-namespaces]
            (try
              (require macro-ns)
              (catch Exception e
                (throw (ex-info (format "failed to require macro-ns:%s, it was required by:%s" macro-ns name) {:ns-info ast} e)))))))

      (if (contains? macro-namespaces name)
        (let [macros (find-macros-in-ns name)]
          (assoc ast :macros macros))
        ast))))

(defn infer-macro-require
  "infer (:require [some-ns]) that some-ns may come with macros
   must be used after load-macros"
  [{:keys [requires] :as ast}]
  (reduce
    (fn [ast [used-name used-ns]]
      (let [macros (get-in @env/*compiler* [::ana/namespaces used-ns :macros])]
        (if (nil? macros)
          ast
          (update-in ast [:require-macros] assoc used-name used-ns)
          )))
    ast
    requires))

(defn infer-macro-use
  "infer (:require [some-ns :refer (something)]) that something might be a macro
   must be used after load-macros"
  [{:keys [uses] :as ast}]
  (reduce
    (fn [ast [used-name used-ns]]
      (let [macros (get-in @env/*compiler* [::ana/namespaces used-ns :macros])]
        (if (or (nil? macros)
                (not (contains? macros used-name)))
          ast
          (update-in ast [:use-macros] assoc used-name used-ns)
          )))
    ast
    uses))

(defn check-uses! [env uses]
  (doseq [[sym lib] uses]
    (when (and (= (get-in @env/*compiler* [::ana/namespaces lib :defs sym] ::not-found) ::not-found)
               (not (contains? (get-in @env/*compiler* [::ana/namespaces lib :macros]) sym)))
      (throw
        (ana/error env
          (ana/error-message :undeclared-ns-form {:type "var" :lib lib :sym sym}))))))
