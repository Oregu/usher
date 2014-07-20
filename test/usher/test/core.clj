(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(deftest t-forward
  (is (= (gen-p [{:fn first} {:fn identity}] {:fn inc})
         (list {:fn inc} {:fn first} {:fn identity})))

  (is (= (synth-p 0
                  [{:fn identity :ar 1}]
                  {:fn zero :ar 0})
         (list {:prog [{:fn zero :ar 0}]})))

  (is (= (forward 0
                  (list {:prog [{:fn identity :ar 1}]})
                  [{:fn zero :ar 0} {:fn first :ar 1}])
         (list {:prog [{:fn zero :ar 0}]}
               {:prog [{:fn identity :ar 1}]})))

  (is (= (forward 1
                  (list {:prog [{:fn identity :ar 1}]})
                  [{:fn zero :ar 0} {:fn first :ar 1}])
         (list {:prog [{:fn first :ar 1} {:fn identity :ar 1}]}
               {:prog [{:fn identity :ar 1}]}))))
