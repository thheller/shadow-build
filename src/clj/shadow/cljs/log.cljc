(ns shadow.cljs.log
  (:require [clojure.pprint :refer (pprint)]))

(defmulti event->str
  (fn [event] (:type event))
  :default ::default)

(defmethod event->str ::default
  [event]
  (with-out-str
    (pprint event)))

(defmethod event->str :compile-cljs
  [{:keys [name] :as event}]
  (format "Compile CLJS: %s" name))

(defmethod event->str :compile-modules
  [event]
  "Compiling modules")

(defmethod event->str :compile-sources
  [{:keys [source-names] :as event}]
  (format "Compiling %d sources" (count source-names)))

(defmethod event->str :cache-read
  [{:keys [name] :as event}]
  (format "[CACHE] read: %s" name))

(defmethod event->str :cache-write
  [{:keys [name] :as event}]
  (format "[CACHE] write: %s" name))

(defmethod event->str :flush-unoptimized
  [{:keys [name] :as event}]
  "Flushing unoptimized modules")

(defmethod event->str :flush-optimized
  [{:keys [path] :as event}]
  (format "Flushing optimized modules" path))

(defmethod event->str :flush-module
  [{:keys [name js-name js-size] :as event}]
  (format "Flushing: %s (%d bytes)" js-name js-size))

(defmethod event->str :flush-sources
  [{:keys [source-names] :as event}]
  (format "Flushing %s sources" (count source-names)))

(defmethod event->str :find-resources
  [{:keys [path] :as event}]
  (format "Finding resources in: %s" path))

(defmethod event->str :find-resources-classpath
  [{:keys [path] :as event}]
  (format "Finding resources in classpath" path))

(defmethod event->str :optimize
  [{:keys [path] :as event}]
  (format "Optimizing" path))

(defmethod event->str :bad-resource
  [{:keys [msg] :as event}]
  msg)

(defmethod event->str :reload
  [{:keys [action ns name file]}]
  (format "RELOAD: %s" name))

(defmethod event->str :warning
  [{:keys [name warning]}]
  (let [{:keys [msg line column]} warning]
    (str "WARNING: " msg " (" name " at " line ":" column ") ")))

(defmethod event->str :name-violation
  [{:keys [src expected url]}]
  (let [{:keys [name ns source-path]}
        src]
    (str "File violation: \"" name "\" produced unexpected ns \"" ns "\""
         "\n\tExpected: " source-path "/" expected
         "\n\tProvided: " source-path "/" name
         )))


