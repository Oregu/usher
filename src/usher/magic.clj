(ns usher.magic
  (:use [usher.core]
        [usher.util]))

(defn zero [] 0)
(defn one  [] 1)

(defn gen-length []
  "Generate Length recursive program."
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

(defn gen-sum []
  "Generate Sum recursive program."
  (run
   [[ ] [2] [3 2]]                    ; input
   [ 0   2    5  ]                    ; output
   [{:fn zero   :ar 0 :name "zero"  } ; components
    {:fn empty? :ar 1 :name "empty?"}
    {:fn first  :ar 1 :name "first" }
    {:fn rest   :ar 1 :name "rest"  }
    ;; TODO don't pass, self should be internal!
    {:fn :self  :ar 1 :name "sum"   }
    {:fn +      :ar 2 :name "+"     }]))

(defn magic-sum []
  (print-p (gen-sum)))

(defn gen-fib []
  "Generate Fibonacci program."
  (run
   [0 1 2 3 4 5]
   [0 1 1 2 3 5]
   [#_{:fn =     :ar 2 :name "="  }
    #_{:fn zero  :ar 1 :name "0"  }
    #_{:fn one   :ar 1 :name "1"  }
    {:fn dec   :ar 1 :name "dec"}
    {:fn #(<= % 1) :ar 1 :name "<= 1" }
    {:fn :self :ar 1 :name "fib"}
    {:fn +     :ar 2 :name "+"  }]))

(defn magic-fib []
  (print-p (gen-fib)))
