(ns usher.test.util
  (:use [usher.util]
        [clojure.test]))

(deftest t-extract
  (is (= (extract-p {:prog [{:fn 'f1 :ar 1} {:fn 'f2 :ar 2}]})
         '(f1 f2)))
  (is (= (extract-p [:if
                     {:prog [{:fn 'fc}]}
                     {:prog [{:fn 'ft}]}
                     {:prog [{:fn 'fe1} {:fn 'fe2}]}])
         '(:if (fc) (ft) (fe1 fe2)))))
