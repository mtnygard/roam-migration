(ns org
  (:require [clojure.string :as str]
            [db]
            [roam])
  (:import  [java.nio.file Path]
            [java.time LocalDate]
            [java.time.format DateTimeFormatter DateTimeParseException]))

(defn- warn [& args]
  (println (apply format args)))

;;; A big feature of Roam is the "daily notes" page. It is a named
;;; page with the spelled-out date as its title:
;;; e.g., "March 6th, 2023".
;;;
;;; We need to do some (ick) date parsing and formatting.

(def ^:private human-date (DateTimeFormatter/ofPattern "MMMM d['st']['nd']['rd']['th'], uuuu"))
(def ^:private mm-dd-yyyy (DateTimeFormatter/ofPattern "MM-dd-uuuu"))
(def ^:private yyyy-mm-dd (DateTimeFormatter/ofPattern "uuuu-MM-dd"))

(defn- ->date
  "Parse a date with a given format or return nil."
  [s ^DateTimeFormatter fmt]
  (try
    (LocalDate/parse s fmt)
    (catch DateTimeParseException e
      nil)))

(defn- ->string
  "Format a date with a given format"
  [^LocalDate d ^DateTimeFormatter fmt]
  (.format d fmt))

(defn daily?
  "Returns true if the page entity 'looks like' a daily notes page."
  [{:keys [:block/uid] :as page-entity}]
  (when uid
    (some? (->date uid mm-dd-yyyy))))

(defn uid->daily-node-id
  "Returns a proper org-roam daily note filename (without the extension)."
  [uid]
  (when-let [day (->date uid mm-dd-yyyy)]
    (->string day yyyy-mm-dd)))

(defn title->daily-node-id
  "Given a spelled-out date string, return a suitable node id.
   E.g., 'March 6th, 2023' becomes '2023-03-06'."
  [title]
  (when-let [day (->date title human-date)]
    (->string day yyyy-mm-dd)))

(defn ref-to-daily?
  "Given a page title, does it look like a daily notes page?"
  [title]
  (some? (->date title human-date)))

;;;
;;; Roam pages are just text. They don't all make good file names!
;;;
;;; This regex attempts to make Linux, macOS, and Windows filenames
;;; out of arbitrary page titles.
;;;

(defn- node-file-name [title]
  (-> title
      (str/replace #"[\s:;/\"\'\`,?!$%^*\(\)&<>]" "-")))

(defn- title->node-id
  "Make a node id from a title. For a daily, this is the yyyy-mm-dd
  format. For all other nodes, this is the same as the node file name
  today, but might need to change soon. (Probably to UUID.)"
  [title]
  (if (ref-to-daily? title)
    (title->daily-node-id title)
    (node-file-name title)))

;;;
;;; Emitting org-formatted markdown
;;;

(def ^:private org-header "%s %s")
(def ^:private org-bold "*%s*")
(def ^:private org-italic "/%s/")
(def ^:private org-inline-code "~%s~")
(def ^:private org-block-code "\n#+begin_src %s\n%s\n#+end_src\n")
(def ^:private org-internal-link "[[id:%s][%s]]")
(def ^:private org-hyperlink "[[%s][%s]]")
(def ^:private org-tag "[[id:%s][%s]]")
(def ^:private org-image-with-alt "\n#+CAPTION: %s\n[[%s]]\n")
(def ^:private org-image "\n[[%s]]\n")

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

(defmethod emit-segment :source [[_ src & [language]]]
  (format org-block-code language src))

(defmethod emit-segment :internal-link [[_ text]]
  (let [id (if (ref-to-daily? text)
             (title->daily-node-id text)
             (title->node-id text))]
    (format org-internal-link id text)))

(defmethod emit-segment :hyperlink [[_ target & [text]]]
  (format org-hyperlink target text))

(defmethod emit-segment :tag [[_ tag]]
  (format org-tag tag tag))

(defmethod emit-segment :attr [[_ attr]]
  (format org-tag attr attr))

(defmethod emit-segment :image [[_ target & [alt]]]
  (if alt
    (format org-image-with-alt alt target)
    (format org-image target)))

(defmethod emit-segment :default [[_ s]]
  s)

(defn- segment-separator [t1 t2]
  (cond
    (= :block-code t2) "\n"
    (= :block-code t1) "\n"
    :else              ""))

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
  (let [segments (roam/parse s)]
    (tap> segments)
    (emit segments)))

;;;
;;; Dealing with org-roam node paths and dailies
;;;

;;; Ordinary nodes get a file with a "cleaned" version of the entity's
;;; title
;;;
;;; Daily notes go in a subdir "daily" and are named like "yyyy-MM-dd"
;;;
;;; This causes some headaches when generating internal (i.e.,
;;; org-roam) links. Those links need to use a node ID, which isn't
;;; the file name, but the ID property of the target node.
;;;
;;; However, in regular Roam text, links to daily notes don't use
;;; their internal system representation. Instead it's written
;;; like "March 6th, 2023". We have to replace that with
;;; [[id:2023-03-06][March 6th, 2023]] in the output.


(defn daily-path
  "Generate a file name for a daily note."
  [dir page-entity]
  (when-let [note-name (uid->daily-node-id (:block/uid page-entity))]
    (str (Path/of dir (into-array String ["daily" (str note-name ".org")])))))

(defn node-path
  [dir {:keys [node/title] :as page-entity}]
  (when-let [node-name (node-file-name (:node/title page-entity))]
    (str (Path/of dir (into-array String [(str node-name ".org")])))))

;;
;; Conversion routines
;;

(defn- children [block-entity]
  (sort-by :block/order (:block/children block-entity)))

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
    (roam-markup->org-markup (:block/string block-entity))))

(defn- format-children [db depth block-entity]
  (list* (indent-by depth (format-block-text db block-entity))
         (mapv #(format-children db (inc depth) %) (children block-entity))))

(defn- classify-page
  [_ page-entity]
  (cond
    (daily? page-entity)  :daily
    :else                 :general))

(def ^:private frontmatter ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s\n")

(defn- format-frontmatter [title]
  (let [id (title->node-id title)]
    (format frontmatter id title)))

(defmulti format-note classify-page)

(defmethod format-note :daily [db page-entity]
  (str/join \newline
            (flatten
              (list*
                (format-frontmatter (:block/uid page-entity))
                (mapv #(format-children db 1 %) (children page-entity))))))


(defmethod format-note :default [db page-entity]
  (str/join \newline
            (flatten
              (list*
                (format-frontmatter (:node/title page-entity))
                (mapv #(format-children db 1 %) (children page-entity))))))


