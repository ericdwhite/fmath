(ns fmath.core
  (:use fmath.server,
        fmath.randwalk,
        fmath.bigote)
  (:gen-class))

(defn -main [& args]
  (run-server {:port 8080}))
