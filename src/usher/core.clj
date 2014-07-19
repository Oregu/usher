(ns usher.core
  (:use clojure.pprint))

(def ^:dynamic *usher-debug* false)

(defn init [in out]
  {:syn   [[{:fn identity
             :ar 1
             :name "i"}]],  ; Collection of synthesized programs.
                            ;; (bipartite) Goal graph:
   :graph {:goals  [out],   ; Goals (programs' valuations).
           :resolvers [],   ; Resolvers connecting goals to subgoals.
           :edges     [],   ; Edges connecting goals and resolvers.
           :root     out}   ; Root, a result we should produce.
                            ;; Set of examples:
   :ex    [in out]})        ; Input and output vectors.

(defn combine [v1 v2]
  "Utility. Combine two vectors."
  (if (empty? v2)
    v1
    (apply conj v1 v2)))

;; Saturate

;; TODO do saturation
(defn oracle [val ind in out]
  ;; TODO well-defined relation, condition is wrong too. Rethink
  (if (and (> (count (last in)) (count val)) (not= (in ind) val))
    (reduce
     #(if (= (first %2) val)
        (second %2)
        %1)
     :noval
     (map list in out))
    :err))

(defn wrap-p [p in out]
  (fn [arg ind]
    (try
      (reduce #(if (pos? (:ar %2))
                (if (= (:fn %2) :self)
                  (oracle %1 ind in out)
                  ((:fn %2) %1))
                ((:fn %2)))
              arg
              (reverse p)) ; f(g(h(args))) or f(g(h))
      (catch Throwable t :err))))

(defn eval-p [p in out] ; TODO TEMP in out, rethink
  "Evaluates program p given inputs vector in. Returns :err on error."
  (vec (map-indexed #((wrap-p p in out) %2 %1) in)))

;; Forward

;; TODO only size 0 and 1 supported
(defn gen-p [c p]
  "Generate new program with component and existing programs."
  (vec (cons c p)))

(defn synth-p [size component programs]
  "Synthesizes new program by applying components of given size
  to existing programs."
  (if (pos? size)
    ;; TODO component should apply to existing goals
    ;; TODO and generate final programs by graph walking.
    (mapv #(gen-p component %) programs)
    [[component]]))

(defn forward [size ps comps]
  ;; TODO don't generate progs that return all errors
  ;; TODO Store evaluations for speed
  (reduce
   (fn [syn c]
     (if (= (:ar c) size)
       (combine syn (synth-p size c ps))
       syn))
   ps
   comps))

;; Split goal

(defn g-intersect [g1 g2]
  "Returns boolean vector with truth in places of intersected positions.
  If no interse found, returns nil."
  (let [inter (mapv = g1 g2)]
    (if (and (some true? inter) (not (every? true? inter))) inter)))

(defn g-intersects [g goals]
  "Returns bool vector of intersections between goal g and each of the goals.
  (With g.)"
  (reduce #(if-let [inter (g-intersect g %2)]
             (conj %1 [inter g])
             %1)
          [] goals))

(defn g-conds [goals evals]
  "Produces _arbitrary_ cond goals."
  (reduce (fn [conds goal]
            (if-let [g-inters (seq (g-intersects goal evals))]
              (apply conj conds g-inters)
              conds))
          []
          goals))

(defn g-then-else [cond+g]
  "For a cond goal vector, build bthen and belse goals.
  Return [condg bthen belse]."
  (let [[cnd g] cond+g
        bthen (map-indexed (fn [ind itm] (if (true?  itm) (g ind) :?)) cnd)
        belse (map-indexed (fn [ind itm] (if (false? itm) (g ind) :?)) cnd)]
    (if (or (= [:? :? :?] belse) (= [:? :? :?] bthen))
      nil ; TODO We don't want 'don't care' goals.
          ; Need not generate them at all
      [g cnd (vec bthen) (vec belse)])))

(defn add-resolver [ifgoals graph]
  "Returns a new graph with resolvers for ifgoals."
  (let [rslvr (keyword (str "r" (count (:resolvers graph))))]
    (-> graph
      ; Add goals: gcond, bthen, belse
      (update-in [:goals] #(conj % (ifgoals 1)))
      (update-in [:goals] #(conj % (ifgoals 2)))
      (update-in [:goals] #(conj % (ifgoals 3)))
      ; Add fresh resolver
      (update-in [:resolvers] #(conj % rslvr))
      ; Add 4 edges: (r, g), (gcond, r), (bthen, r), (belse, r)
      (update-in [:edges] #(conj % [rslvr (ifgoals 0)]))
      ; TODO here should be indexes of goals and resolvers
      (update-in [:edges] #(conj % [(ifgoals 1) rslvr]))
      (update-in [:edges] #(conj % [(ifgoals 2) rslvr]))
      (update-in [:edges] #(conj % [(ifgoals 3) rslvr])))))

(defn split-g [usher]
  "SplitGoal rule, adds more resolvers to the goal graph."
  (let [graph (:graph usher)
        n (count (:root graph))
        ex (:ex usher)
        in  (ex 0)
        out (ex 1)
        gconds (g-conds (:goals graph)
                        (map #(eval-p % in out) (:syn usher)))
        ifgoals (map g-then-else gconds)]
    (reduce #(if %2 (add-resolver %2 %1) %1) graph ifgoals)))

;; Resolve

(defn equal-g [g1 g2]
  (every?
   #(or
     (= (first %1) (second %1))
     (some (partial = :?) [(first %1) (second %1)]))
   (map list g1 g2)))

(defn match-g [ps g in out]
  "Find programs in ps which match goal g on input in."
  (some #(equal-g g (eval-p % in out)) ps))

(defn edges [rslvr graph]
  "Find resolver's edges in graph."
  (let [edges (:edges graph)]
    (reduce #(if (= rslvr (first %2))
               (conj %1 (second %2))
               (if (= rslvr (second %2))
                 (conj %1 (first %2))
                 %1)) [] edges)))

(defn resolve-p [r usher]
  "Resolve r with programs ps with given graph."
  (let [es (edges r (:graph usher))
        rg (es 0)
        g1 (es 1) ; TODO bad
        g2 (es 2)
        g3 (es 3)
        in  ((:ex usher) 0)
        out ((:ex usher) 1)
        ps (:syn usher)
        p1 (some #(if (equal-g g1 (eval-p %1 in out)) %1) ps)
        p2 (some #(if (equal-g g2 (eval-p %1 in out)) %1) ps)
        p3 (some #(if (equal-g g3 (eval-p %1 in out)) %1) ps)]
    ;; TODO rg= is a temp termination condition
    ;; This should check for resolvance of some resolver,
    ;; Not termination.
    (if (and p1 p2 p3
             (= rg (:root (:graph usher))))
      [:if p1 p2 p3])))

;; Termination

;; TODO termination is when root goal resolved.

(defn print-p
  ([p] (print-p p 0))
  ([p indent]
     (do
       (print (apply str (repeat indent " ")))
       (if (= :if (first p))
         (do
           (print "if ")
           (print-p (p 1))
           (println)
           (print "  then ")
           (print-p (p 2))
           (println)
           (print "  else ")
           (print-p (p 3))
           (println))
         (doall
          (map #(print (:name %) "") p))))))

;; Entry-point

(defn run* [in out comps]
  {:pre [(= (count in) (count out))]}
  (loop [usher (init in out)
         size  0]
    (let [fwd   (forward size (:syn usher) comps)
          usher (assoc usher :syn fwd)
          graph (split-g usher)
          usher (assoc usher :graph graph)
          rslvd (reduce
                 (fn [ps rslvr]
                   (if-let [p (resolve-p rslvr usher)]
                     (conj ps p)
                     ps))
                 []
                 (get-in usher [:graph :resolvers]))]
      (if *usher-debug* (pprint usher))

      (if (seq rslvd)
        (doall (print-p (first rslvd)))
        (recur usher
               ;; TODO size
               1 #_(inc size))))))


(defn zero [] 0)

(defn magic-length []
  "Generates length recursive function."
  (run*
    [[ ] [2] [1 2]]                    ; input
    [ 0   1    2  ]                    ; output
    [{:fn zero   :ar 0 :name "zero"  } ; components with arity a(c)
     {:fn empty? :ar 1 :name "empty?"}
     {:fn inc    :ar 1 :name "inc"   }
     {:fn first  :ar 1 :name "first" }
     {:fn rest   :ar 1 :name "rest"  }
     ; TODO don't pass, self should be internal!
     {:fn :self  :ar 1 :name "length"}]))

(defn magic-fib []
  "Generate Fibonacci program."
  (run*
   [0 1 2 3 4 5 6]
   [0 1 1 2 3 5 8]
   [{:fn +     :ar 2 :name "+"  }
    {:fn :self :ar 1 :name "fib"}]))
