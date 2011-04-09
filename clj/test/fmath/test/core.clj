(ns fmath.test.core
  (:use [fmath.core])
  (:use [clojure.test]))

(deftest test-default-walk 
  (is (= (count
           (default-walk 3)) 3)
      "Invalid walk length"))
