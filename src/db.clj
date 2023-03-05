(ns db
  (:require [datascript.core :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader Reader]
           [java.util.zip ZipFile]))

;;;
;;; I/O
;;;

(defn load-db
  "Read a datascript database from a reader that supplies EDN."
  [^Reader rdr]
  (edn/read {:readers d/data-readers} (PushbackReader. rdr)))

(defn load-db-file
  "Read a datascript database from an EDN file"
  [^String f]
  (load-db (io/reader f)))

(defn load-db-from-zip
  "Read a datascript database from an EDN file inside a Zip archive"
  [zip-path entry-name]
  (with-open [zip (ZipFile. zip-path)]
    (let [stream (.getInputStream zip entry-name)]
      (load-db (io/reader stream)))))

;;;
;;; Logical model
;;;

(defn page-content-by-title
  "Returns zero or one entities with the following keys, where `title` matches `:node/title`:
    :db/id
    :block/uid
    :node/title
    :block/string
    :block/order
    :block/refs
    :block/children"
  [db title]
  (ffirst
    (d/q '[:find (pull ?e [:db/id :block/uid :node/title :block/string :block/order :block/refs :block/children {:block/children ...}])
           :in $ ?page-title
           :where [?e :node/title ?page-title]]
         db title)))

(defn refs-by-block-id
  "Returns a set of refs"
  [db uid]
  (d/q '[:find ?r
         :in $ ?uid
         :where
         [?b :block/uid ?uid]
         [?b :block/refs ?r]]
       db uid))

(defn ref-titles-by-db-ids
  "Returns pairs of [ref-id title] for the input refs. If the ref doesn't point to a page (i.e., it is a block ref), title will be nil."
  [db refs]
  (d/q '[:find (pull ?db-id  [:db/id (:node/title :default "")])
         :in $ [?db-id ...]]
       db (map :db/id refs)))


;
