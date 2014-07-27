(ns usher.test.core
  (:refer-clojure :exclude [resolve])
  (:use [usher.core]
        [clojure.test]))

(defn numb [n] (constantly n))
(def zero (numb 0))
(def one  (numb 1))
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
        dec {:fn dec   :ar 1}
        lt1 {:fn #(<= % 1) :ar 1}
        pls {:fn +     :ar 2}
        self {:fn :self :ar 1}
        withzr (update-in usher [:syn] conj {:prog [zr] :val [0 0 0]})
        withfv (update-in usher [:syn] conj {:prog [fv] :val [5 5 5]})]

    (is (= (gen-p [zr fst] inc)
           (list inc zr fst)))

    (is (= (gen-p [zr fst] pls)
           (list pls (list zr fst))))

    (is (= (synth-p progid zr)
           [{:prog [zr]}])
        "0 arity should not stack with existing programs.")

    (is (= (synth-p [{:prog [zr]} {:prog [fst]}] pls)
           [{:prog [pls [[zr] [zr]]]}
            {:prog [pls [[zr] [fst]]]}
            {:prog [pls [[fst] [zr]]]}
            {:prog [pls [[fst] [fst]]]}])
        ">1 arity program should synthesize all possible programs.")

    (is (= (:syn (forward [zr fst] usher))
           [progid
            {:prog [zr]     :val [0 0 0]}
            {:prog [fst id] :val [1 2 3]}]))

    (is (= (:syn (forward [inc] withzr))
           [progid
            {:prog [zr]     :val [0 0 0]}
            {:prog [inc zr] :val [1 1 1]}]))

    (is (= (:syn (forward [fst pls] withfv))
           [progid
            {:prog [fv]     :val [5 5 5]}
            {:prog [fst id] :val [1 2 3]}
            {:prog [pls [[fv] [fv]]]         :val [10 10 10]}
            {:prog [pls [[fv] [fst id]]]     :val [6 7 8]}
            {:prog [pls [[fst id] [fst id]]] :val [2 4 6]}]))

    (let [in    [0 1 2 3 4 5]
          out   [0 1 1 2 3 5]
          usher (init in out)
          fwd1  (forward [dec lt1 self pls] usher)
          usher (merge-with into usher fwd1)
          fwd1  (:syn fwd1)
          fwd2  (forward [dec lt1 self pls] usher)
          usher (merge-with into usher fwd2)
          fwd2  (:syn fwd2)
          usplt (split usher)
          goals (:goals (:graph usplt))
          rslv  (resolve usplt)
          urslv (merge-with into usplt rslv)
          fwd3  (:syn urslv)
          ifs   (filter #(= :if (first (:prog %))) fwd3)]

      (is (some #(= (:prog %) [dec id]) fwd1))
      (is (some #(= (:prog %) [self dec id]) fwd1))
      (is (some #(= (:prog %) [dec dec id]) fwd2))
      (is (some #(= (:prog %) [self dec dec id]) fwd2))
      (is (some #(= (:prog %) [pls (list [self dec id]
                                         [self dec dec id])]) fwd2))

      (is (some #(= % [true true false false false false]) goals))

      (is (some #(= (:val %) [true true false false false false]) fwd2))
      (is (some #(= (:val %) [0 1 2 3 4 5]) fwd2))
      (is (some #(= (:val %) [:noval 0      1 1 2 3]) fwd2)) ; fib(n-1)
      (is (some #(= (:val %) [:noval :noval 0 1 1 2]) fwd2)) ; fib(n-2)
      (is (some #(= (:val %) [:err   :err   1 2 3 5]) fwd2)) ; +

      (is (some #(let [eds (edges % (:graph urslv))]
                   (and
                    (= (eds 0) [0 1 1 2 3 5])
                    (= (eds 1) [true true false false false false])
                    (= (eds 2) [0 1 :? :? :? :?])
                    (= (eds 3) [:? :? 1 2 3 5])))
                (get-in urslv [:graph :resolvers])))

      (is (some #(= (:prog %) [:if
                               [lt1 id]
                               [id]
                               [pls (list [self dec id]
                                          [self dec dec id])]]) ifs))
      (is (= (:prog (terminate (:root (:graph urslv)) (:syn urslv)))
             [:if
              [lt1 id]
              [id]
              [pls (list [self dec id]
                         [self dec dec id])]])))))

(deftest t-eval
  (is (= (eval-p [{:fn first :ar 1}] [[] [2] [1 2]] nil)
         [nil 2 1])
      "Program should evaluate.")

  (is (= (eval-p [{:fn + :ar 2}
                  (list [{:fn five     :ar 0}]
                        [{:fn first    :ar 1}
                         {:fn identity :ar 1}])]
                 [[1] [2 3] [4]]
                 nil)
         [6 7 9])
      ">1 arity program should evaluate.")

  (is (= (eval-p [:if
                  [{:fn empty? :ar 1}]
                  [{:fn zero   :ar 0}]
                  [{:fn first  :ar 1}]] [[] [2] [1 2]] nil)
         [0 2 1])
      "If program should evaluate.")

  (is (= (eval-p [:if
                  [{:fn <= :ar 2}
                   (list [{:fn identity :ar 1}]
                         [{:fn one      :ar 0}])]
                  [{:fn identity :ar 1}]
                  [{:fn + :ar 2}
                   (list [{:fn :self :ar 1}
                          {:fn dec   :ar 1}]
                         [{:fn :self :ar 1}
                          {:fn dec   :ar 1}
                          {:fn dec   :ar 1}])]]
                 [0 1 2 3 4 5 6 7]
                 [0 1 1 2 3 5 8 13])
         [0 1 1 2 3 5 8 13])
      "Fibonacci should evaluate."))

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
        usher (split usher)]

    (is (= (last (:syn (resolve usher)))
           {:prog [:if (:prog emp) (:prog zr) (:prog fst)]
            :val [0 2 1]}))))

(deftest t-oracle

  (is (= (oracle 3 1 [1 2 3 4] [11 22 33 44])
         33))

  (is (= (oracle [2] 0 [[] [2] [4 3]] [0 2 4])
         2)))
