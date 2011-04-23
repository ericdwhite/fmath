(ns fmath.randwalk
  (:use clojure.contrib.json))

(defn flip
  "Uses frand to obtain a number from 0 inclusive,
  to 1 exclusive, and then compares that to the
  limit to determine head or tails.  Adjusting
  the limit is changes the number of heads vs. 
  tails."
  ([] (flip rand 0.5)) 
  ([frand limit] (let [r (frand)]
          (cond
            (>= r limit) true
            :else false))))

(defn step [cur, up, dn]
  (if (flip)
    (* cur up)
    (* cur dn)))

(defn pace [up, dn]
  #(step % up dn))

(defn walk [steps, start, up, dn]
  (take steps (iterate (pace up dn) start)))

;;
;; Used for creating results sets to 
;; display on the graph.
(defn default-walk [steps]
  "Runs a basic walk with a starting point
  of 50 and 8% change up or down."
  (map #(vector %1 %2) 
       (iterate inc 0)
       (walk steps 50 1.08 0.92)))
       
(defn json-default-walk [steps]
  (json-str (default-walk steps)))
