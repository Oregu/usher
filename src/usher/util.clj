(ns usher.util)

(defn indent [n]
  (print (apply str (repeat n " "))))

(defn print-p
  ([p] (print-p p false 0))
  ([p wrap] (print-p p wrap 0))
  ([p wrap n]
     (do
       (indent n)
       (if (= :if (first p))
         (do
           (print "if")
           (print-p (p 1))
           (println)
           (indent n)
           (print "  then")
           (print-p (p 2))
           (println)
           (indent n)
           (print "  else")
           (print-p (p 3))
           (println))
         (do
          (if wrap (print " ("))
          (doall (map (fn [fun]
                        (if (map? fun)
                          (print "" (:name fun))
                          (doall (map #(if (string? %1)
                                         (print %1)
                                         (print-p %1 true))
                                      (interpose "," fun)))))
                      p))
          (if wrap (print ")")))))))

(defn extract-fn [fun]
  (if (map? fun)
    (:fn fun)
    (map extract-fn fun)))

(defn extract-p [p]
  (if (= :if (first p))
    (cons :if (map extract-p (rest p)))
    (map extract-fn p)))
