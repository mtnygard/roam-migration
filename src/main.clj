(ns main
  (:require [db]
            [org]
            [clojure.tools.cli :as cli]
            [clojure.java.io :as io])
  (:import [java.io File]))

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

(defn- generate-files [{:keys [db dest] :as options}]
  (let [title           "March 2nd, 2023"
        one-page        (db/page-content-by-title db title)
        output-path     (org/daily-path dest one-page)
        output-contents (org/format-note one-page)]
    (println "Writing " output-path)
    (println output-contents)
    (spit output-path output-contents)))

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
