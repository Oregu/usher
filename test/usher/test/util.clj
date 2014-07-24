(ns usher.test.util
  (:use [usher.util]
        [clojure.test]))

(deftest t-extract
  (is (= (extract-p [{:fn 'f1 :ar 1} {:fn 'f2 :ar 2}])
         '(f1 f2)))
  (is (= (extract-p [:if
                     [{:fn 'fc}]
                     [{:fn 'ft}]
                     [{:fn 'fe1} {:fn 'fe2}]])
         '(:if (fc) (ft) (fe1 fe2)))))
