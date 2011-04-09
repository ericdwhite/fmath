(ns fmath.core
  (:use clojure.contrib.json)
  (:gen-class))

(defn frand []
  (rand))

(defn flip
  ([] (flip frand)) 
  ([fr] (let [r (fr)]
          (cond
            (> r 0.5) true
            :else false))))

(defn step [cur, up, dn]
  (if (flip)
    (* cur up)
    (* cur dn)))

(defn walk [steps, start, up, dn]
  (loop [result [] count steps acc start]
    (if (zero? count)
      result
      (let [next (step acc up dn)]
        (recur (conj result next) (dec count) next)))))

(defn default-walk [steps]
  (let [result (walk steps 100 1.05, 0.95)]
    (loop [acc [] x 0 ycol result]
      (if (empty? ycol)
        acc
        (recur (conj acc [x (first ycol)]) (inc x) (rest ycol))))))

(defn json-default-walk [steps]
  (json-str (default-walk steps)))

(defn -main [& args]
  (apply json-default-walk args))

