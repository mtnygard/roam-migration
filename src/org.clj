(ns org
  (:import [java.nio.file Files Path])
  (:require [clojure.string :as str]))

(def ^:private mm-dd-yyyy? #"^[0-9]{2}-[0-9]{2}-[0-9]{4}$")
(def ^:private mm-dd-yyyy #"([0-9]{2})-([0-9]{2})-([0-9]{4})")

(defn daily?
  "Returns true if the page entity 'looks like' a daily notes page."
  [page-entity]
  (when-let [uid (:block/uid page-entity)]
    (re-matches mm-dd-yyyy? uid)))

(defn daily-note-name
  "Returns a proper org-roam daily note filename (without the extension)."
  [page-entity]
  (some-> page-entity
          :block/uid
          (->> (re-matches mm-dd-yyyy))
          rest
          reverse
          (->> (str/join "-"))))

(defn daily-path
  "Generate a file name for a daily note."
  [dir page-entity]
  (when-let [note-name (daily-note-name page-entity)]
    (str (Path/of dir (into-array String [(str note-name ".org")])))))

(defn- header [n]
  (str/join (repeat n "*")))

(defn- indent-by [n text]
  (format "%s %s" (header n) text))

(def ^:private roam-emphasis #"\*\*([^*]*)\*\*")
(def ^:private org-emphasis "*$1*")

(def ^:private roam-italics #"__([^_]*)__")
(def ^:private org-italics "/$1/")

(def ^:private roam-code #"`([^`]*)`")
(def ^:private org-code "~$1~")

;; this regex for hyperlinks is pretty confusing
;; it looks for [some text](some target), capturing the first and
;; second parts.
;;
;; it probably breaks if the text or the target contain parens or
;; brackets.
;; then again, i don't know how Roam even handles that!
(def ^:private roam-hyperlink #"\[([^\]]*)\]\(([^)]*)\)")
(def ^:private org-hyperlink "[[$2][$1]]")


(defn- literal?
  "Check if the text is a blockquote form"
  [s]
  (str/starts-with? s "```"))

(defn- roam-markup->org-markup [s]
  (if (literal? s)
    s
    (-> s
        (str/replace roam-emphasis org-emphasis)
        (str/replace roam-italics org-italics)
        (str/replace roam-code org-code)
        (str/replace roam-hyperlink org-hyperlink))))

(defn- format-block-text [block-entity]
  ;; todo - split the block text into a seq of markup & blockquotes
  (roam-markup->org-markup (:block/string block-entity)))

(defn- format-children [depth block-entity]
  (list* (indent-by depth (format-block-text block-entity))
         (mapv #(format-children (inc depth) %) (:block/children block-entity))))

(defn- classify-page
  [page-entity]
  (cond
    (daily? page-entity)  :daily
    :else                 :general))

(defmulti format-note classify-page)

(defmethod format-note :daily [page-entity]
  ;;; lots of work is needed here
  (let [blocks (:block/children page-entity)
        blocks (sort-by :block/order blocks)]
    (str/join \newline
              (flatten (list* (format "title: %s" (daily-note-name page-entity))
                              (mapv #(format-children 1 %) blocks))))))


(defmethod format-note :default [page-entity]
  (let [blocks (:block/children page-entity)
        blocks (sort-by :block/order blocks)]
    (str/join \newline
              (flatten (list* (format "title: %s" (:node/title page-entity))
                              (mapv #(format-children 1 %) blocks)))))
  )
