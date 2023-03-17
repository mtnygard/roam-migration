(ns io
  (:require [clojure.java.io :as jio])
  (:import [java.io File]))

(defn ensure-directory [^String path]
  (jio/make-parents path))

(defn file-exists? [^String path]
  (.exists (File. path)))

(defn directory? [^String path]
  (.isDirectory (File. path)))
