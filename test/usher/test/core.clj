(ns usher.test.core
  (:use [usher.core]
        [clojure.test]))

(defn numb [n] (constantly n))
(def zero (numb 0))
(def five (numb 5))

(deftest t-forward
  (let [inv    [[1] [2] [3]]
        outv   nil
        usher  (init inv outv)
        progid (first (:syn usher))
        id  (first (:prog progid))
        zr  {:fn zero  :ar 0}
        fv  {:fn five  :ar 0}
        fst {:fn first :ar 1}
        inc {:fn inc   :ar 1}
        pls {:fn +     :ar 2}
        withzr (update-in usher [:syn] conj {:prog [zr] :val [0 0 0]})
        withfv (update-in usher [:syn] conj {:prog [fv] :val [5 5 5]})]

    (is (= (gen-p [zr fst] inc)
           (list inc zr fst)))

    (is (= (gen-p [zr fst] pls)
           (list pls (list zr fst))))

    (is (= (synth-p progid zr)
           (list {:prog [zr]})))

    (is (= (synth-p [{:prog [zr]} {:prog [fst]}] pls)
           (list {:prog [pls [[zr]
                              [fst]]]})))

    (is (= (:syn (forward [zr fst] usher))
           [progid
            {:prog [zr]     :val '(0 0 0)}
            {:prog [fst id] :val '(1 2 3)}]))

    (is (= (:syn (forward [inc] withzr))
           [progid
            {:prog [zr]     :val '(0 0 0)}
            {:prog [inc zr] :val '(1 1 1)}]))

    (is (= (:syn (forward [fst pls] withfv))
           [progid
            {:prog [fv]     :val '(5 5 5)}
            {:prog [fst id] :val '(1 2 3)}
            {:prog [pls [[fv] [fst id]]]
             :val '(6 7 8)}]))))

(deftest t-eval
  (is (= (eval-p [{:fn first :ar 1}] [[] [2] [1 2]] nil)
         [nil 2 1]))

  (is (= (eval-p (list {:fn + :ar 2}
                       (list [{:fn five     :ar 0}]
                             [{:fn first    :ar 1}
                              {:fn identity :ar 1}]))
                 [[1] [2 3] [4]]
                 nil)
         [6 7 9]))

  (is (= (eval-p [:if
                  [{:fn empty? :ar 1}]
                  [{:fn zero   :ar 0}]
                  [{:fn first  :ar 1}]] [[] [2] [1 2]] nil)
         [0 2 1])))

(deftest t-resolve
  (let [in    [[] [2] [1 2]]
        out   [0 2 1]
        usher (init in out)
        progid (first (:syn usher))
        id  (first (:prog progid))
        zr  {:prog [{:fn zero   :ar 0}] :val [0 0 0]}
        emp {:prog [{:fn empty? :ar 1}] :val [true false false]}
        fst {:prog [{:fn first  :ar 1}] :val [:err 2 1]}
        usher (update-in usher [:syn] conj zr emp fst)
        usher (split-g usher)]

    (is (= (last (:syn (resolve-g usher)))
           {:prog [:if (:prog emp) (:prog zr) (:prog fst)]
            :val [0 2 1]}))))
