(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(deftest t-forward
  (let [id  {:prog [{:fn identity :ar 1}]}
        zr  {:fn zero  :ar 0}
        fst {:fn first :ar 1}
        inc {:fn inc }]

    (is (= (gen-p [zr fst] inc)
           (list inc zr fst)))

    (is (= (synth-p 0
                    [{:fn identity :ar 1}]
                    {:fn zero :ar 0})
           (list {:prog [{:fn zero :ar 0}]})))

    (is (= (forward 0 (list id) [zr fst])
           (list {:prog [zr]} id)))

    (is (= (forward 1 (list {:prog [id]}) [zr fst])
          (list {:prog [fst id]} {:prog [id]})))))
