(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(deftest t-forward
  (let [id  {:fn identity :ar 1}
        zr  {:fn zero     :ar 0}
        fst {:fn first    :ar 1}
        inc {:fn inc      :ar 1}
        inv [[1] [2] [3]]
        progid {:prog [id] :val inv}]

    (is (= (gen-p [zr fst] inc)
           (list inc zr fst)))

    (is (= (synth-p 0 progid zr)
           (list {:prog [zr]})))

    (is (= (forward 0 (list progid) [zr fst] inv [])
           (list {:prog [zr] :val [0 0 0]} progid)))

    (is (= (forward 1 (list progid) [zr fst] inv [])
           (list {:prog [fst id] :val '(1 2 3)} progid)))

    (is (= (forward 1 (list {:prog [zr] :val '(0 0 0)} progid) [inc] inv [])
           (list {:prog [inc zr] :val '(1 1 1)}
                 {:prog [zr] :val '(0 0 0)}
                 progid)))))

(deftest t-eval
  (is (= (eval-p [{:fn first :ar 1}] [[] [2] [1 2]] [0 1 2])
         [nil 2 1])))
