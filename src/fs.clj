(ns fs
  (:require [clojure.java.io :as jio])
  (:import [java.io File]
           [java.nio.file Path]))

(defn ensure-directory [^String path]
  (jio/make-parents path))

(defn file-exists? [^String path]
  (.exists (File. path)))

(defn directory? [^String path]
  (.isDirectory (File. path)))

(defn extend [^String path & exts]
  (.toString (Path/of path (into-array String exts))))

