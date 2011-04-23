(ns fmath.core
  (:use fmath.server,
        fmath.randwalk,
        fmath.bigote)
  (:gen-class))

(defn -main [& args]
  (run-server {:host "127.0.0.1",
               :port 8080}))
