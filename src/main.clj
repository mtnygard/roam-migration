(ns main
  (:require [db]
            [org]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io])
  (:import [java.io File]))

(defn- success [msg]
  (spit "success.txt" msg :append true))

(defn- error [msg]
  (spit "errors.txt" msg :append true))

(defn- verify-options [{:keys [source dest] :as options}]
  (when-not source
    (println "A source database or zip file is necessary."))
  (when-not dest
    (println "A destination directory is necessary."))
  options)

(defn- read-database [{:keys [source] :as options}]
  (let [db (db/load-db-file source)]
    (assert db "Something went wrong loading the database")
    (assoc options :db db)))

(def ^:dynamic *batch-size* 100)

(defn- file-exists? [path]
  (.exists (File. path)))

(defn- generate-files [{:keys [db dest] :as options}]
  (let [library (db/page-ids db)
        batches (partition-all *batch-size* library)]
    (println (format "%d pages to process, in %d batches of %d" (count library) (count batches) *batch-size*))
    (doseq [b    batches
            id   b
            :let [p (db/page-content-by-id db id)]]
      (try
        (let [output-path (if (org/daily? p) (org/daily-path dest p) (org/node-path dest p))
              contents    (org/format-note db p)]
          (if (file-exists? output-path)
            (error (format "%s: file named %s already exists\n" id output-path))
            (do
              (spit output-path contents)
              (success (format "%s\n" (:node/title p))))))
        (catch Exception e
          (error (format "%s: %s\n" id (or (.getMessage e) (type e)))))))))

(def cli-options
  [["-s" "--source FILE" "Source file, in EDN format"]
   ["-d" "--dest DIRECTORY" "Location where .org files will be placed"
    :validate [#(.isDirectory (File. %)) "Destination must be an existing directory"]]
   ["-h" "--help" "Print this help"]])

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)]
    (if errors
      (binding [*out* *err*]
        (doseq [e errors]
          (println e)))
      (if (:help options)
        (do
          (println "Usage: roam-migrate - convert a RoamResearch export into a directory of org-roam files")
          (println summary))
        (-> options
            verify-options
            read-database
            generate-files)))))
