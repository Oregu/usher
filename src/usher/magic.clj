(ns usher.magic
  (:use [usher.core]
        [usher.util]))

(defn zero [] 0)

(defn gen-length []
  "Generates length recursive function."
  (run
   [[ ] [2] [1 2]]                    ; input
   [ 0   1    2  ]                    ; output
   [{:fn zero   :ar 0 :name "zero"  } ; components with arity a(c)
    {:fn empty? :ar 1 :name "empty?"}
    {:fn inc    :ar 1 :name "inc"   }
    {:fn first  :ar 1 :name "first" }
    {:fn rest   :ar 1 :name "rest"  }
    ;; TODO don't pass, self should be internal!
    {:fn :self  :ar 1 :name "length"}]))

(defn magic-length []
  (print-p (gen-length)))

(defn gen-fib []
  "Generate Fibonacci program."
  (print-p
   (run
    [0 1 2 3 4 5 6]
    [0 1 1 2 3 5 8]
    [{:fn +     :ar 2 :name "+"  }
     {:fn =     :ar 2 :name "="  }
     {:fn dec   :ar 1 :name "dec"}
     {:fn :self :ar 1 :name "fib"}])))

(defn magic-fib []
  (print-p (gen-fib)))

(defn gen-sum []
  "Generates sum recursive function."
  (run
   [[ ] [2] [3 2]]                    ; input
   [ 0   2    5  ]                    ; output
   [{:fn zero   :ar 0 :name "zero"  } ; components
    {:fn +      :ar 2 :name "+"     }
    {:fn empty? :ar 1 :name "empty?"}
    {:fn first  :ar 1 :name "first" }
    {:fn rest   :ar 1 :name "rest"  }
    ;; TODO don't pass, self should be internal!
    {:fn :self  :ar 1 :name "sum"}]))

(defn magic-sum []
  (print-p (gen-sum)))
