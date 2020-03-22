(ns crapp.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.tools.cli :as cli])
  (:import
   (java.io File)))

(def conf
  {:chrome/bin "/opt/google/chrome/google-chrome"
   :chrome/directory ".config/google-chrome"
   :chrome/profile "Default"})

(defn user-profile-directory [{:chrome/keys [directory profile]}]
  (str/join File/separator [(System/getProperty "user.home") directory profile]))

(defn find-manifests [start-dir filename]
  (loop [[f & rest] (.listFiles (io/as-file start-dir)) manifests [] file-count 0]
    (cond
      (and (nil? f) (empty? rest))
      (do
        (println (str/join " " ["Traversed" file-count "files and found" (count manifests) "manifests."]))
        manifests)

      (and (.isFile f) (= filename (.getName f)))
      (recur rest (conj manifests f) (inc file-count))
      
      (.isFile f)
      (recur rest manifests (inc file-count))

      (.isDirectory f)
      (recur (concat rest (.listFiles f)) manifests file-count)

      :else
      (recur rest manifests file-count))))

(defn manifest-index []
  (println "Finding and building manifest index...")
  (let [crapp-cache-file (io/as-file (str/join File/separator [(System/getProperty "user.home") ".config" "crapp.edn"]))
        crapp-cache (when (.exists crapp-cache-file) (clojure.edn/read-string (slurp crapp-cache-file)))]
    (cond
      crapp-cache
      crapp-cache
      
      :else
      (let [the-index
            (->>
             (find-manifests (user-profile-directory conf) "manifest.json")
             (map
              (fn [mf]
                {:name (-> mf slurp json/read-str clojure.walk/keywordize-keys :name)
                 :app-id (-> mf .getParentFile .getParentFile .getName)}))
             (remove
              (fn [{:keys [name]}]
                (or
                 (.startsWith name "__")
                 (.startsWith name "http"))))
             distinct
             (sort-by :name))]
        (spit crapp-cache-file the-index)
        the-index))))

(defn start-app [name]
  (let [apps (manifest-index)
        app (first (filter (fn [mf] (= name (:name mf))) apps))]
    (cond
      app
      (do 
        (println "found app: " app)
        (println "launching...")
        (shell/sh
         (:chrome/bin conf)
         (str "--profile=" (:profile-directory conf))
         (str "--app-id=" (:app-id app))))
      
      :else 
      (throw
       (ex-info
        "No app found with that name."
        {:name name
         :available-apps (map :name apps)})))))

(def cli-opts
  [["-a" "--app-name APP_NAME" "Launches app having APP_NAME, if available."]
   ["-d" "--debug" "Enable debug"]
   ["-h" "--help" "Prints help and exit."]
   ["-i" "--interactive" "Start with interactive mode"]
   ["-l" "--list-apps" "Lists apps"]])

(defn print-help [opts]
  (println (str "#=== HELP ===#"))
  (println (:summary opts)))

(defn -main [& args]
  (let [{:keys [options] :as parsed-opts} (cli/parse-opts args cli-opts)]
    (println (str "#=== Chrome Apps Launcher ===#"))
    (when (:debug options) (println "got: " (dissoc parsed-opts :summary)))
    
    (cond
      (:errors parsed-opts)
      (do
        (print-help parsed-opts)
        (println "-----------------")
        (println "(-_-) Error launching: " (:errors parsed-opts))
        (println "ktnxbye!"))
      
      (:help options)
      (print-help parsed-opts)
      
      (:app-name options)
      (start-app (:app-name options))
      
      (:list-apps options)
      (println "Available Apps:" (with-out-str (clojure.pprint/pprint (map :name (manifest-index)))))
      
      :else
      (println "Hmm. This is unexpected. Try -h"))))