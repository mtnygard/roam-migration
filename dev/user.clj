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

  (def march-02-2023 (page-content-by-title "March 2nd, 2023" roam-db))
  (def march-03-2023 (page-content-by-title "March 3rd, 2023" roam-db))
  (def dawn-demo (page-content-by-title "Dawn Demo" roam-db))

  (require 'org)
  (require 'db)

  march-03-2023
  
  (org/daily? march-02-2023)

  (org/daily-note-name march-02-2023)

  (org/daily-path "tmp" march-02-2023)

  (org/format-note roam-db march-02-2023)

  (spit (io/file "dawn-demo.org") (org/format-note roam-db dawn-demo))

  (db/ref-titles-by-db-ids roam-db [{:db/id 42062} {:db/id 42652}])

  (d/pull roam-db '[*] 89171)

  (d/q '[:find ?e ?t :where [?e :node/title ?t]] roam-db)

  (def why (db/page-content-by-id roam-db 24212 ))

  (org/daily-note-name (:block/uid why))

  (org/format-note roam-db why)

  (sort-by :block/order (get-in why [:block/children 0 :block/children]))

  (org/format-note roam-db (db/page-content-by-id roam-db 4013))

  (first (db/page-ids roam-db))

  (d/q '[:find ?e
         :where [?e :block/uid "1OlOye32X"]] roam-db)
  
  dawn-demo
  )
