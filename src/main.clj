(ns main
  (:require [db]
            [org]
            [io]
            [fs]
            [roam]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files Path]))

(defn- success [msg]
  (spit "success.txt" msg :append true))

(defn- error [msg]
  (spit "errors.txt" msg :append true))

(defn- fatal [errors]
  (binding [*out* *err*]
    (doseq [e errors]
      (println e))))

(defn- read-database [{:keys [source] :as options}]
  (let [db (db/load-db-file source)]
    (assert db "Something went wrong loading the database")
    (assoc options :db db)))

(defn- asset-path [^String p]
  (fs/extend
      (if (str/ends-with? p ".org")
        (subs p 0 (- (count p) 4))
        p)
    "assets"))

(defn hyperlinks-in-block [block-entity]
  (for [s     (roam/parse (:block/string block-entity))
        :when (some #{(first s)} #{:image :hyperlink :macro})]
    (let [[tp arg1 & [arg2]] s]
      (if (and (= :macro tp) (= "pdf" arg1))
        arg2
        arg1))))

(defn- local-path [^String url]
  (cond
    (firebase/firebase-url? url) (firebase/firebase-url->local-filename url)
    :else nil))

(defn downloads
  "Return a {url -> [filesystem_path relative_path]}"
  [page-entity ^String fs-dest ^String relative-dest]
  ;; 1. reduce over a tree-seq of the page & its children
  ;; 2. roam/parse each string
  ;; 3. look for :hyperlink or :image segments
  ;; 4. match the domain of the hyperlink target
  ;; 5. construct a local file name
  ;; 6. assoc the hyperlink and the local file name into the map
  (reduce
    (fn [downloads hyperlink]
      (if-let [local (local-path hyperlink)]
        (assoc downloads
               hyperlink
               [(fs/extend fs-dest local) (fs/extend relative-dest local)])
        downloads))
    {}
    (mapcat hyperlinks-in-block (roam/block-seq page-entity))))

(def ^:dynamic *batch-size* 100)

(defn- download-assets! [linkmap]
  (doseq [[url [local _]] linkmap]
    (fs/ensure-directory local)
    (firebase/download-asset! url local)
    (success (format "download to %s\n" local))))

(defn- safe-write! [id title output-path contents]
  (if (fs/file-exists? output-path)
    (error (format "%s: file named %s already exists\n" id output-path))
    (do
      (fs/ensure-directory output-path)
      (spit output-path contents)
      (success (format "%s\n" title)))))

(defn- generate-files [{:keys [db dest range] :as options}]
  (let [library (db/page-ids db)
        batches (partition-all *batch-size* library)]
    (println (format "%d pages to process, in %d batches of %d" (count library) (count batches) *batch-size*))
    (doseq [b    batches
            id   b
            :let [p (db/page-content-by-id db id)]]
      (try
        (let [storage  (org/page-storage dest p)
              dl       (downloads p (:asset-dir storage) (:asset-relative-dir storage))
              contents (org/format-note db dl p)]
          (download-assets! dl)
          (safe-write! id (:node/title p) (:org-file storage) contents))
        (catch Exception e
          (error (format "%s: %s\n" id (or (.getMessage e) (type e)))))))))

(defn- execute [options]
  (-> options
      read-database
      generate-files))

(def cli-options
  [["-s" "--source FILE" "Source file, in EDN format"]
   ["-d" "--dest DIRECTORY" "Location where .org files will be placed"]
   ["-h" "--help" "Print this help"]])

(defn- help [summary]
  (println "Usage: roam-migrate - convert a RoamResearch export into a directory of org-roam files")
  (println summary))

(defn- verify-options [{:keys [source dest] :as options}]
  (when-not source
    (fatal ["A source database or zip file is necessary."]))
  (when-not dest
    (fatal ["A destination directory is necessary."]))
  (and (some? source) (some? dest)))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)]
    (if errors
      (fatal errors)
      (if (:help options)
        (help)
        (if (verify-options options)
          (execute options))))))
