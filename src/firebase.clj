(ns firebase
  (:require [clojure.string :as str]
            [fs]
            [clojure.java.io :as io])
  (:import [java.net URL]
           [java.nio.file Path]))

(def ^:private buffer-size (* 1024 1024))

(defn url-path-string [^String u]
  (.getPath (URL. u)))

;; This whole function is very sensitive to the exact structure of
;; Roam's firebase asset URLs
(defn firebase-url->local-filename
  [^String fb-url]
  (when fb-url
    (let [without-query (url-path-string fb-url)]
      (if-let [sep (str/last-index-of without-query "%2F")]
        (subs without-query (+ 3 sep))
        without-query))))

(defn firebase-url? [^String url]
  (str/starts-with? url "https://firebasestorage.googleapis.com"))

(defn download-asset!
  [^String fb-url ^String dest-path]
  (with-open [out (io/output-stream dest-path)
              in  (io/input-stream (URL. fb-url))]
    (io/copy in out :buffer-size buffer-size)))

