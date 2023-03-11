(ns user
  (:require [datascript.core :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defn schema [db]
  (d/schema db))

(defn page-content-by-title [t db]
  (ffirst
    (d/q '[:find (pull ?e [:db/id :block/uid :node/title :block/string :block/order :block/heading :block/refs :block/page
                           :children/view-type :block/children {:block/children ...}])
           :in $ ?page-title
           :where [?e :node/title ?page-title]]
         db t)))

(defn page-id-by-title [t db]
  (ffirst
    (d/q '[:find ?e
           :in $ ?page-title
           :where [?e :node/title ?page-title]]
         db t)))


(defn block-content-by-dbid
  [db dbid]
  (d/pull db
          '[:db/id :block/uid :node/title :block/string :block/order :block/refs :block/children {:block/children ...}]
          dbid))


(comment

  (def roam-db
    (with-open [rdr (io/reader "/home/mtnygard/Downloads/Nygards2ndBrain.edn")]
      (edn/read {:readers d/data-readers} (PushbackReader. rdr))))

  (def march-08-2023 (page-content-by-title "March 8th, 2023" roam-db))

  (require 'org)
  (require 'db)

  (org/format-note roam-db march-08-2023)

  (org/format-note roam-db (page-content-by-title "Satisfactory Storage Room Layout" roam-db))

  (def room (page-content-by-title "Satisfactory Storage Room Layout" roam-db))
  (spit
    (io/file "tmp/test.org")
    (org/format-note roam-db march-08-2023))

  ;; the table in this page works OK
  (org/format-note roam-db room)

  ;; the first table in this page demonstrates three bugs with
  ;; formatting.
  ;; 1. newlines in the cell break the org-mode format.
  ;; 2. the table doesn't really have a header row in Roam, but the
  ;;    org-mode table does (Roam gives no indication of what makes a
  ;;    row a header.
  ;; 3. because of the newline problem, the column width is calculated
  ;;    incorrectly (it uses the string length rather than the line length
  (org/format-note roam-db
                   (page-content-by-title "Documenting Software Architectures: Views and Beyond" roam-db))

  )
