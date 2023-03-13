(ns org
  (:require [clojure.string :as str]
            [db]
            [roam])
  (:import  [java.nio.file Path]
            [java.time LocalDate]
            [java.time.format DateTimeFormatter DateTimeParseException]))

(defn- string-length-proper [s]
  (if (some? s)
    (.codePointCount s 0 (count s))
    0))

(defn- org-bold? [s]
  (seq (re-matches #"\*[^\*]+\*" s)))

(defn- looks-like-header-row? [cols]
  (every? org-bold? cols))

(defn- looks-like-hrule? [{:keys [block/string]}]
  (= string ":hiccup [:hr]"))

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
    (when s
      (LocalDate/parse s fmt))
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
;;; Roam page titles are just text. They don't all make good file names!
;;;
;;; This regex attempts to make valid Linux, macOS, and Windows filenames
;;; out of arbitrary page titles.
;;;

(defn- node-file-name [title]
  (-> title
      (str/replace #"[\s:;/\"\'\`,?!$%^*\(\)&<>]+" "-")))

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
(def ^:private org-hrule  "-----")
(def ^:private org-bold "*%s*")
(def ^:private org-italic "/%s/")
(def ^:private org-strikethrough "+%s+")
(def ^:private org-inline-code "~%s~")
(def ^:private org-block-code "\n#+begin_src %s\n%s\n#+end_src\n")
(def ^:private org-internal-link "[[id:%s][%s]]")
(def ^:private org-hyperlink "[[%s][%s]]")
(def ^:private org-tag "[[id:%s][%s]]")
(def ^:private org-image-with-alt "\n#+CAPTION: %s\n[[%s]]\n")
(def ^:private org-image "\n[[%s]]\n")
(def ^:private org-todo " TODO ")
(def ^:private org-done " DONE ")
(def ^:private org-video-link "[[%s]]")

(defmulti emit-segment first)

(defmethod emit-segment :text/header [[_ s level]]
  (format org-header (str/join (repeat level "*")) s))

(defmethod emit-segment :text/plain [[_ s]]
  ;; special handling for a convention I use to make a separator
  (if (= s ":hiccup [:hr]")
    (format org-hrule)
    s))

(defmethod emit-segment :text/bold [[_ s]]
  (format org-bold s))

(defmethod emit-segment :text/italic [[_ s]]
  (format org-italic s))

(defmethod emit-segment :text/strikethrough [[_ s]]
  (format org-strikethrough s))

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

(defmethod emit-segment :macro [[m kind & [arg]]]
  (cond
    (= "TODO" kind)  (format org-todo)
    (= "DONE" kind)  (format org-done)
    (= "video" kind) (format org-video-link arg)
    (= "pdf" kind)   (format org-hyperlink arg "")
    :else            (str kind arg)))

;; Since org-mode has no equivalent to Roam's ^^highlight^^ markup,
;; we allow segment type :highlight to be handled in the default case.
(defmethod emit-segment :default [[_ s]]
  s)

(defn- text-segment? [t]
  (when t
    (= "text" (namespace t))))

(defn- text-joining-code? [t1 t2]
  (or
    (and (text-segment? t1) (= :inline-code t2))
    (and (:inline-code t1) (text-segment? t2))))

;; Org-mode formatting has some quirks about spacing between different spans
(defn- segment-separator [t1 t2]
  (cond
    (text-joining-code? t1 t2) " "
    (= :block-code t2)         "\n"
    (= :block-code t1)         "\n"
    :else                      ""))

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

(defn- has-children? [block-entity]
  (not (empty? (:block/children block-entity))))

(defn- short-heading? [block-entity]
  (< (count (:block/string block-entity)) 80))

(defn- chrepeat [n c] (str/join (repeat n c)))

(defn- indent-by [text n c]
  (format "%s %s" (str/join (repeat n c)) text))

(defn- format-as-bullet [text depth index]
  (format "%s %s"
          (chrepeat depth "*")
          text))

(defn- format-as-numbered-list [text depth index]
  (format "%s%d. %s"
          (chrepeat (* 3 (dec (or depth 1))) " ")
          index
          text))

(def ^:private format-as-document identity)

(defn- table? [[segment-type segment-content & [_]]]
  (and (= :macro segment-type)
       (= "table" segment-content)))

;; This little function does a lot
;;
;; The `iterate` call returns an infinite lazy seq of the first child's first child
;; and its first child, etc. The `take-while some?` part cuts off the
;; infinite seq so we keep the non-nil portion.
;;
;; Instead of returning a seq of block-entities, we parse the Roam
;; text from the blocks and return a vec of org-mode strings.
;;
;; Since Roam allows embedded newlines in a cell but org doesn't, we
;; replace any newlines with a Unicode paragraph mark as a
;; placeholder.

(defn collect-row [block-entity]
  (mapv
    (fn [block]
      (-> block
          :block/string
          roam/parse
          emit
          (str/replace #"\n" "¶")))
    (take-while some?
                (iterate (comp first :block/children) block-entity))))

;; transpose-with-fill
;;
;; transpose a vector-of-vectors The simpler, idiomatic `(apply map
;; vector coll)` cuts off any columns that aren't "full" in every row.
(defn transpose [rows]
  (let [nrows (count rows)
        ncols (apply max (map count rows))]
    (for [icol (range ncols)]
      (for [irow (range nrows)]
        (get-in rows [irow icol])))))

(defn lazy-pad
  "Returns a lazy sequence which pads sequence with pad-value."
  [sequence pad-value]
  (if (empty? sequence)
    (repeat pad-value)
    (lazy-seq (cons (first sequence) (lazy-pad (rest sequence) pad-value)))))

(defn format-table
  [rows]
  (let [header?   (looks-like-header-row? (first rows))
        txpose    (transpose rows)
        widths    (mapv
                    (fn [col]
                      (apply max (map string-length-proper col)))
                    txpose)
        spacers   (map #(apply str (repeat % "-")) widths)
        fmts      (map #(str "%-" (if (= 0 %) 1 %) "s") widths)
        fmt-row   (fn [leader divider trailer vs fmts]
                    (str leader
                         (apply str
                                (interpose divider
                                           (map format fmts vs)))
                         trailer))]
    (if header?
      (list* (fmt-row "| " " | " " |" (first rows) fmts)
             (fmt-row "|-" "-+-" "-|" (first rows) spacers)
             (for [row (rest rows)]
               (fmt-row "| " " | " " |" (lazy-pad row "") fmts)))
      (mapv #(fmt-row "| " " | " " |" (lazy-pad % "") fmts) rows))))

(defn format-block-and-children [db depth index view-type block-entity]
  (let [kids          (children block-entity)
        parent?       (not (empty? kids))
        heading?      (and (= :bulleted view-type)
                           (or parent?
                               (short-heading? block-entity))
                           (not (looks-like-hrule? block-entity)))
        numbered?     (= :numbered view-type)
        document?     (= :document view-type)
        kid-view-type (or (:children/view-type block-entity) :bulleted)
        segments      (roam/parse (:block/string block-entity))]
    (if (table? (first segments))
      (let [table-contents (mapv collect-row kids)]
        (format-table table-contents))
      (let [self (emit segments)]
        (cond-> self
          heading?  (format-as-bullet depth index)
          numbered? (format-as-numbered-list depth index)
          document? (format-as-document)
          parent?   (list*
                      (doall
                        (map-indexed
                          #(format-block-and-children
                             db
                             (inc depth)
                             (inc %1)
                             kid-view-type
                             %2)
                          kids))))))))

(def ^:private frontmatter ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s")

(defn- format-frontmatter [title]
  (let [id (title->node-id title)]
    (format frontmatter id title)))

(defn- page-title [page-entity]
  (if (daily? page-entity)
    (:block/uid page-entity)
    (:node/title page-entity)))

(defn format-note [db page-entity]
  (when page-entity
    (str/join \newline
              (flatten
                (list*
                  (format-frontmatter (page-title page-entity))
                  (format-block-and-children db 0 0 :bulleted page-entity))))))

