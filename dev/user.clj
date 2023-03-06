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
                           :block/children {:block/children ...}])
           :in $ ?page-title
           :where [?e :node/title ?page-title]]
         db t)))

(defn page-id-by-title [t db]
  (ffirst
    (d/q '[:find ?e
           :in $ ?page-title
           :where [?e :node/title ?page-title]]
         db t)))

(comment

  (def roam-db
    (with-open [rdr (io/reader "/home/mtnygard/Downloads/Nygards2ndBrain.edn")]
      (edn/read {:readers d/data-readers} (PushbackReader. rdr))))

  (def march-06-2023 (page-content-by-title "March 6th, 2023" roam-db))

  (require 'org)
  (require 'db)

  (org/daily? march-06-2023)

  (org/daily-note-name march-06-2023)

  (org/daily-path "tmp" march-06-2023)

  (org/format-note roam-db march-06-2023)

  (spit (io/file "tmp/test/2023-03-06.org") (org/format-note roam-db march-06-2023))

  )
