(ns org
  (:require [clojure.string :as str]
            [db])
  (:import  [java.nio.file Path]))

;;;
;;; Emitting org-formatted markdown
;;;

(def ^:private org-header "%s %s")
(def ^:private org-bold "*%s*")
(def ^:private org-italic "/%s/")
(def ^:private org-inline-code "~%s~")
(def ^:private org-block-code "#+begin_src %s\n%s\n#+end_src")
(def ^:private org-internal-link "[[id:%s][%s]]")
(def ^:private org-hyperlink "[[%s][%s]]")
(def ^:private org-tag "[[id:%s][%s]]")

(defmulti emit-segment first)

(defmethod emit-segment :text/header [[_ s level]]
  (format org-header (str/join (repeat level "*")) s))

(defmethod emit-segment :text/plain [[_ s]]
  s)

(defmethod emit-segment :text/bold [[_ s]]
  (format org-bold s))

(defmethod emit-segment :text/italic [[_ s]]
  (format org-italic s))

(defmethod emit-segment :inline-code [[_ s]]
  (format org-inline-code s))

(defmethod emit-segment :block-code [[_ src & [language]]]
  (format org-block-code language src))

(defmethod emit-segment :internal-link [[_ target & [id]]]
  (format org-internal-link id target))

(defmethod emit-segment :hyperlink [[_ target & [text]]]
  (format org-hyperlink target text))

(defmethod emit-segment :tag [[_ tag]]
  (format org-tag tag tag))

(defmethod emit-segment :attr [[_ attr]]
  (format org-tag attr attr))

(defmethod emit-segment :default [[_ s]]
  s)

(defn- segment-separator [t1 t2]
  (cond
    (nil? t1)          ""
    (= :block-code t2) "\n"
    (= :block-code t1) "\n"
    :else              " "))

(defn emit [segments]
  (loop [output       ""
         previous-seg nil
         seg          (first segments)
         remainder    (rest segments)]
    (if-not seg
      output
      (let [output (format "%s%s%s"
                           output
                           (segment-separator (first previous-seg) (first seg))
                           (emit-segment seg))]
        (recur output seg (first remainder) (rest remainder))))))

(defn- roam-markup->org-markup [s]
  s)

;;;
;;; Dealing with org-roam node paths and dailies

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

(defn- format-block-text [db block-entity]
  ;; todo - split the block text into a seq of markup & blockquotes
  ;; todo - look for roam internal links, confirm the count matches
  ;; the number of refs for the block
  (let [refs (db/refs-by-block-id db (:block/uid block-entity))]
    (assert (= (count refs) (count (:block/refs block-entity))))
    (roam-markup->org-markup (:block/string block-entity))
    ))

(defn- format-children [db depth block-entity]
  (list* (indent-by depth (format-block-text db block-entity))
         (mapv #(format-children db (inc depth) %) (:block/children block-entity))))

(defn- classify-page
  [_ page-entity]
  (cond
    (daily? page-entity)  :daily
    :else                 :general))

(defmulti format-note classify-page)

(defmethod format-note :daily [db page-entity]
  ;;; lots of work is needed here
  (let [blocks (:block/children page-entity)
        blocks (sort-by :block/order blocks)]
    (str/join \newline
              (flatten (list* (format "title: %s" (daily-note-name page-entity))
                              (mapv #(format-children db 1 %) blocks))))))


(defmethod format-note :default [db page-entity]
  (let [blocks (:block/children page-entity)
        blocks (sort-by :block/order blocks)]
    (str/join \newline
              (flatten (list* (format "title: %s" (:node/title page-entity))
                              (mapv #(format-children db 1 %) blocks)))))
  )
