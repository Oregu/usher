(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(def zero (constantly 0))

(deftest t-forward
  (let [inv    [[1] [2] [3]]
        outv   nil
        usher  (init inv outv)
        progid (first (:syn usher))
        id  (first (:prog progid))
        zr  {:fn zero  :ar 0}
        fst {:fn first :ar 1}
        inc {:fn inc   :ar 1}
        withzr (update-in usher [:syn] conj {:prog [zr] :val [0 0 0]})]

    (is (= (gen-p [zr fst] inc)
           (list inc zr fst)))

    (is (= (synth-p progid zr)
           (list {:prog [zr]})))

    (is (= (:syn (forward [zr fst] usher))
           [progid
            {:prog [zr]     :val '(0 0 0)}
            {:prog [fst id] :val '(1 2 3)}]))

    (is (= (:syn (forward [inc] withzr))
           [progid
            {:prog [zr]     :val '(0 0 0)}
            {:prog [inc zr] :val '(1 1 1)}]))))

(deftest t-eval
  (is (= (eval-p [{:fn first :ar 1}] [[] [2] [1 2]] [0 1 2])
         [nil 2 1])))
