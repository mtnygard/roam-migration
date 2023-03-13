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
    (with-open [rdr (io/reader "../../Downloads/Nygards2ndBrain.edn")]
      (edn/read {:readers d/data-readers} (PushbackReader. rdr))))

  (def march-08-2023 (page-content-by-title "March 8th, 2023" roam-db))

  (require 'org)
  (require 'db)

  (org/format-note roam-db march-08-2023)

  (org/format-note roam-db (page-content-by-title "Attention is All You Need" roam-db))

  (spit
    (io/file "tmp/test.org")
    (org/format-note roam-db march-08-2023))

  ;; This page has an embedded PDF (which Roam put into firebase for me.)
  (def attn (page-content-by-title "Attention is All You Need" roam-db))

  ;; This page has a screenshot in it (which Roam also put into
  ;; firebase for me, but with a different format in the Roam string)
  (def feb-17-2023 (page-content-by-title "February 17th, 2023" roam-db))
  ;; block with :db/id 88597 has such content
  (d/pull roam-db '[*] 88597)
  img-url
  (roam/parse img-url)
  )
