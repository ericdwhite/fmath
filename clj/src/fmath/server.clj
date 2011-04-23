(ns fmath.server
  (:use clojure.contrib.json,
        compojure.core,
        ring.adapter.jetty,
        fmath.bigote
        fmath.randwalk)
  (:require [compojure.route :as route])
  (:gen-class))

(defn show-index []
  (mustache-execute 
    (mustache-compile "index.html")
    { "title-intro" "Launch",
     "title" "Let's rock",
     "content" "{{content}}"}))

(defn show-404 []
          "<h1>Page Not Found</h1>")

(defn- json-resp [data]
  {:status 200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body data})

;;
;; Random Walk
;;
(defn random-walk []
  (json-resp (json-default-walk 100)))

(defroutes main-routes
  (GET "/" [] (show-index))
  (GET "/math/randomwalk/" [] (random-walk))
  (route/files "/" {:root "public"})
  (route/not-found (show-404)))

(defn run-server [params]
  (run-jetty main-routes params))
