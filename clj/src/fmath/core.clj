(ns fmath.core
  (:use fmath.server,
        fmath.randwalk,
        fmath.bigote)
  (:import (org.mortbay.jetty.handler AbstractHandler)
           (org.mortbay.jetty.handler RequestLogHandler)
           (org.mortbay.jetty NCSARequestLog))
  (:gen-class))

(def production?
  (= "production" (get (System/getenv) "APP_ENV")))

(def development?
  (not production?))

(defn app-ip []
  (get (System/getenv) "APP_IP" "127.0.0.1"))

(defn app-port []
  (Integer/parseInt (get (System/getenv) "APP_PORT" "8080")))

(defn- request-log-hander []
  "See: 
    http://jetty.codehaus.org/jetty/jetty-6/apidocs/org/mortbay/jetty/NCSARequestLog.html"
  (let [log (new NCSARequestLog "logs/jetty-yyyy_mm_dd.request.log")
        handler (new RequestLogHandler)]
    (doto log
      (.setRetainDays 90)
      (.setAppend true)
      (.setExtended false)
      (.setLogLatency true)
      (.setLogTimeZone "UTC"))
    (.setRequestLog handler log)
    handler))

(defn- log-configurator [server]
  (.addHandler server (request-log-hander)))

(defn server [join configurator]
  (run-server {:configurator configurator
               :host (app-ip)
               :port (app-port)
               :join? join}))

(defn -main [& args]
  (server true log-configurator))

