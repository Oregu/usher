(ns usher.test.magic
  (:use [usher.magic]
        [usher.util]
        [clojure.test]))

(deftest t-length
  (is (= (extract-p (gen-length))
         [:if
          [empty? identity]
          [zero]
          [inc :self rest identity]])))
