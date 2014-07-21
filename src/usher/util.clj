(ns usher.util)

(defn print-p
  ([p] (print-p p 0))
  ([p indent]
     (do
       (print (apply str (repeat indent " ")))
       (if (= :if (first p))
         (do
           (print "if ")
           (print-p (:prog (p 1)))
           (println)
           (print "  then ")
           (print-p (:prog (p 2)))
           (println)
           (print "  else ")
           (print-p (:prog (p 3)))
           (println))
         (doall
          (map #(print (:name %) "") p))))))

(defn extract-p [p]
  (if (= :if (first p))
    (cons :if (map extract-p (rest p)))
    (->> p :prog (map :fn))))
