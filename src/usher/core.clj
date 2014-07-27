(ns usher.core
  (:use clojure.pprint)
  (:require [clojure.math.combinatorics :as combo]))

(def ^:dynamic *usher-debug* false)

(defn init [in out]
  {:syn   [{:prog [{:fn identity
                    :ar 1
                    :name "i"}]
            :val in}],      ; Collection of synthesized programs.
   :evals #{(repeat (count in) :err)}
                            ; Set of evaluations of synthesized programs
                            ; so ot exclude duplicated programs.
                            ;; (bipartite) Goal graph:
   :graph {:goals  [out],   ; Goals (programs' valuations).
           :resolvers [],   ; Resolvers connecting goals to subgoals.
           :edges     [],   ; Edges connecting goals and resolvers.
           :root     out}   ; Root, a result we should produce.
                            ;; Set of examples:
   :ex    [in out]})        ; Input and output vectors.

;; Saturate

;; No saturation implemented.
;; For Oracle to work for recursive functions,
;; one needs to pass continous list of values.

(defn oracle [val ind in out]
  ;; TODO well-defined relation, condition is wrong too. Rethink
  (if (or (and (coll? (first in))
               (> (count (last in)) (count val))
               (not= (in ind) val))
          (and (number? (first in))
               (> (last in) val)
               (not= (in ind) val)))
    (reduce
     #(if (= (first %2) val) (second %2) %1)
     :noval
     (map list in out))
    :err))

;; Evaluation

(defn wrap-p [p in out]
  (fn [arg ind]
    (try
      (if (= :if (first p))
        (if ((wrap-p (p 1) in out) arg ind)
          ((wrap-p (p 2) in out) arg ind)
          ((wrap-p (p 3) in out) arg ind))  ;
        (reduce (fn [arg fun]                ; TODO What a mess.
                  (if (map? fun)            ;
                    (if (pos? (:ar fun))
                      (if (= (:fn fun) :self)
                        (oracle arg ind in out)
                        (if (= (:ar fun) 1)
                          ((:fn fun) arg)   ; f(g(h(args)))
                          (apply (:fn fun) arg))) ; f(g(h), m(n))
                      ((:fn fun)))          ; f(g(h))
                    (map #((wrap-p % in out) arg ind) fun)))
                arg
                (reverse p)))
      (catch Throwable t :err))))

(defn eval-p [p in out] ; TODO TEMP [in out] for oracle, rethink
  "Evaluates program p given inputs vector in. Returns :err on error."
  (map-indexed #((wrap-p p in out) %2 %1) in))

(defn eval-ps [ps evals in out]
  "Return back a map with programs and updated valuations set.
  No repeats."
  (reduce #(let [ev (eval-p (:prog %2) in out)]
             (if (evals ev)
               %1
               (-> %1
                   (update-in [:ps] conj (assoc %2 :val ev))
                   (update-in [:evals] conj ev))))
          {:ps [] :evals evals}
          ps))

;; Forward

(defn gen-p [p c]
  "Generate new program with component and existing programs."
  (if (= (:ar c) 1)
    (cons c p)
    (list c p)))

(defn synth-p [programs component]
  "Synthesizes new program by applying components of given size
  to existing programs."
  (let [arity (:ar component)
        progs (map :prog programs)]
    (if (> arity (count programs))
      '()
      (if (pos? arity)
        (map (fn [pr] {:prog (gen-p pr component)})
             (if (= arity 1)
               progs
               (combo/selections progs arity)))
        (list {:prog (list component)})))))

(defn forward [comps usher]
  "Synthesize more programs from given programs ps components."
  (let [ps  (:syn usher)
        in  ((:ex usher) 0)
        out ((:ex usher) 1)]
    (reduce
     (fn [usher c]
       (let [synth (synth-p (:syn usher) c)
             evald (eval-ps synth (:evals usher) in out)]
         (-> usher
             (update-in [:syn]   into (:ps evald)) ; TODO DRY
             (update-in [:evals] into (:evals evald)))))
     usher
     comps)))

;; Split goal

(defn true-indexes [cond]
  (loop [arr cond
         ind 0
         acc []]
    (if (empty? arr)
      acc
      (recur (rest arr)
             (inc ind)
             (if (true? (first arr))
               (conj acc ind)
               acc)))))

;; TODO Right now it turns off just one true at a time.
;; Should turn to false permutations of indexes.
(defn arbitrary-g [cond g]
  (let [inds (true-indexes cond)]
    (if (> (count inds) 1)
      (into [[cond g]] (map (fn [i] [(assoc cond i false) g]) inds))
      [[cond g]])))

(defn intersect-g [g1 g2]
  "Returns boolean vector with truth in places of intersected positions.
  If no interse found, returns nil."
  (let [inter (mapv = g1 g2)]
    (if (and (some true?  inter)
             (some false? inter)) inter)))

(defn intersects-g [g evals]
  "Returns bool vector of intersections between goal g
  and each of the evals."
  (reduce #(if-let [inter (intersect-g g %2)]
             (into %1 (arbitrary-g inter g))
             %1)
          [] evals))

(defn g-conds [goals evals]
  "Produces arbitrary cond goals. Intersects goals and evals."
  (reduce (fn [conds goal]
            (if-let [g-inters (seq (intersects-g goal evals))]
              (apply conj conds g-inters)
              conds))
          []
          goals))

(defn branch-g [cond+g]
  "For a cond goal vector, build bthen and belse goals.
  Return [goal cond bthen belse]."
  (let [[cnd g] cond+g
        bthen (map-indexed (fn [ind itm] (if (true?  itm) (g ind) :?)) cnd)
        belse (map-indexed (fn [ind itm] (if (false? itm) (g ind) :?)) cnd)]
    [g cnd (vec bthen) (vec belse)]))

(defn add-resolver [ifgoals graph]
  "Returns a new graph with resolvers for ifgoals."
  (let [rslvr (keyword (str "r" (count (:resolvers graph))))]
    (-> graph
        ;; Add goals: gcond, bthen, belse
        (update-in [:goals] into (rest ifgoals))
        ;; Add fresh resolver
        (update-in [:resolvers] conj rslvr)
        ;; Add 4 edges: (r, g), (gcond, r), (bthen, r), (belse, r)
        (update-in [:edges] conj [rslvr (ifgoals 0)])
        ;; TODO here should be indexes of goals and resolvers
        (update-in [:edges] conj [(ifgoals 1) rslvr])
        (update-in [:edges] conj [(ifgoals 2) rslvr])
        (update-in [:edges] conj [(ifgoals 3) rslvr]))))

(defn split-g [usher]
  "SplitGoal rule, adds resolvers to the goal graph if found."
  (let [graph (:graph usher)
        n (count (:root graph))
        ex (:ex usher)
        in  (ex 0)
        out  (ex 1)
        gconds (g-conds (:goals graph)
                        (map :val (:syn usher)))
        ifgoals (map branch-g gconds)]
    (assoc usher :graph ; TODO Don't repeat resolvers
           (reduce #(if %2 (add-resolver %2 %1) %1) graph ifgoals))))

;; Resolve

(defn equal-g [g1 g2]
  (every? #(or (= (first %1) (second %1))
               (or (= (first %1) :?) (= (second %1) :?)))
          (map list g1 g2)))

(defn edges [rslvr graph]
  "Find resolver's edges in graph."
  (let [edges (:edges graph)]
    (reduce #(if (= rslvr (first %2))
               (conj %1 (second %2))
               (if (= rslvr (second %2))
                 (conj %1 (first %2))
                 %1)) [] edges)))

(defn resolve-p [r usher]
  "Resolve r with programs ps in graph. Give back resolved program."
  (let [es (edges r (:graph usher))
        g1 (es 1) ; TODO bad
        g2 (es 2)
        g3 (es 3)
        ps (:syn usher)
        p1 (some #(if (equal-g g1 (:val %1)) %1) ps)
        p2 (some #(if (equal-g g2 (:val %1)) %1) ps)
        p3 (some #(if (equal-g g3 (:val %1)) %1) ps)]
    (if (and p1 p2 p3)
      [:if (:prog p1) (:prog p2) (:prog p3)])))

(defn resolve-g [usher]
  (let [in  ((:ex usher) 0)
        out ((:ex usher) 1)
        ps (reduce
            (fn [ps rslvr]
              (if-let [p (resolve-p rslvr usher)]
                (conj ps {:prog p})
                ps))
            [] (get-in usher [:graph :resolvers]))
        evald (eval-ps ps (:evals usher) in out)]
    (-> usher
        (update-in [:syn]   into (:ps evald))   ; TODO DRY
        (update-in [:evals] into (:evals evald)))))

;; Termination

;; TODO Return not first but smallest
(defn terminate [root synth]
  "Return program resolving root goal. If one exists."
  (some (fn [pr] (if (= root (:val pr)) pr)) synth))

;; Entry-point

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (loop [usher (init in out)]
    (let [usher (->> usher
                     (forward comps)
                     (split-g)
                     (resolve-g))
          answer (terminate (:root (:graph usher)) (:syn usher))]
      (if *usher-debug* (do (pprint usher) (read-line)))

      (if answer
        (:prog answer)
        (recur usher)))))
