(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(deftest t-forward
  (is (= (gen-p {:fn inc} [{:fn first} {:fn identity}])
         (list {:fn inc} {:fn first} {:fn identity})))

  #_(is (= (synth-p 0
                  {:fn zero :ar 0}
                  [{:fn identity :ar 1}])
         (list {:prog [{:fn zero :ar 0}]
                :val  [0 0 0]}))))
