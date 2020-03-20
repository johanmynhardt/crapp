(ns crapp.core
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell])
  (:import
   (java.io File)))

(def conf
  {:chrome/bin "/opt/google/chrome/google-chrome"
   :chrome/directory ".config/google-chrome"
   :chrome/profile "Default"})

(defn user-profile-directory [{:chrome/keys [directory profile]}]
  (str/join File/separator [(System/getProperty "user.home") directory profile]))

(defn find-manifests [start-dir filename]
  (loop [[f & rest] (.listFiles (io/as-file start-dir)) manifests []]
    (cond
      (and (nil? f) (empty? rest))
      manifests

      (and (.isFile f) (= filename (.getName f)))
      (recur rest (conj manifests f))

      (.isDirectory f)
      (recur (concat rest (.listFiles f)) manifests)

      :else
      (recur rest manifests))))

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

(defn -main [& args]
  (println (str "Chrome Apps Launcher. Apps requested: " args))
  (cond
    (seq args)
    (doseq [app args]
      (start-app app))
    
    :else
    (println "Available Apps:" (with-out-str (clojure.pprint/pprint (map :name (manifest-index)))))))