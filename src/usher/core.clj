(ns usher.core
  (:refer-clojure :exclude [resolve])
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
               (> (count (peek in)) (count val))
               (not= (in ind) val))
          (and (number? (first in))
               (> (peek in) val)
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
  (reduce #(let [val (eval-p (:prog %2) in out)]
             (if ((:evals %1) val)
               %1
               (-> %1
                   (update-in [:syn]   conj (assoc %2 :val val))
                   (update-in [:evals] conj val))))
          {:syn [] :evals evals}
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
        in  (first (:ex usher))
        out (second (:ex usher))]
    (reduce
     (fn [newps c]
       (let [synth (synth-p (:syn newps) c)
             evald (eval-ps synth (:evals newps) in out)]
         (merge-with into newps evald)))
     {:syn ps :evals (:evals usher)}
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

(defn conds-g [goals evals]
  "Produces arbitrary cond goals. Intersects goals and evals."
  (reduce (fn [conds goal]
            (if-let [g-inters (seq (intersects-g goal evals))]
              (apply conj conds g-inters)
              conds))
          [] goals))

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

(defn split [usher]
  "SplitGoal rule, adds resolvers to the goal graph if found."
  (let [graph (:graph usher)
        n (count (:root graph))
        ex (:ex usher)
        in  (ex 0)
        out  (ex 1)
        gconds (conds-g (:goals graph)
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
  (let [edges (:edges graph)
        cnt (count edges)]
    (loop [ind 0]
      (if (= rslvr (first (edges ind)))
        [(second (edges ind))
         (first (edges (inc ind)))
         (first (edges (inc (inc ind))))
         (first (edges (inc (inc (inc ind)))))]
        (if (< ind cnt)
          (recur (inc ind)))))))

(defn find-p [goals evals ps]
  "Find programs resolving goals."
  (let [g1 (goals 0)
        g2 (goals 1)
        g3 (goals 2)]
    (loop [ps ps
           acc [nil nil nil]]
      (if (empty? ps)
        acc
        (let [prog (first ps)
              acc (if (and (nil? (acc 0)) (equal-g g1 (:val prog)))
                    (assoc acc 0 prog) acc)
              acc (if (and (nil? (acc 1)) (equal-g g2 (:val prog)))
                    (assoc acc 1 prog) acc)
              acc (if (and (nil? (acc 2)) (equal-g g3 (:val prog)))
                    (assoc acc 2 prog) acc)]
          (if (not-any? nil? acc)
            acc
            (recur (rest ps) acc)))))))

(defn resolve-p [r usher]
  "Resolve r with programs ps in graph. Give back resolved program."
  (let [es (edges r (:graph usher))
        g1 (es 1) ; TODO bad
        g2 (es 2)
        g3 (es 3)
        ps (:syn usher)
        ex (:ex usher)
        evals (:evals usher)
        [p1 p2 p3] (find-p [g1 g2 g3] evals ps)]
    (if (and p1 p2 p3)
      (let [pif [:if (:prog p1) (:prog p2) (:prog p3)]
            val (eval-p pif (first ex) (second ex))]
        (if (not (evals val))
          {:prog pif :val val})))))

(defn resolve [usher]
  (let [in  ((:ex usher) 0)
        out ((:ex usher) 1)]
    (reduce
     (fn [ps rslvr]
       (if-let [p (resolve-p rslvr usher)]
         (-> ps
             (update-in [:syn]   conj p)
             (update-in [:evals] conj (:val p)))
         ps))
     {:syn [] :evals (:evals usher)}
     (get-in usher [:graph :resolvers]))))

;; Termination

;; TODO Return not first but smallest
(defn terminate [root synth]
  "Return program resolving root goal. If one exists."
  (some (fn [pr] (if (= root (:val pr)) pr)) synth))

;; Entry-point

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (loop [usher (init in out)]
    (let [ps (forward comps usher)
          usher (-> usher
                    (assoc :syn   (:syn   ps))
                    (assoc :evals (:evals ps)))
          usher (split usher)
          synif (resolve usher)
          usher (merge-with into usher synif)
          answer (terminate (:root (:graph usher)) (:syn usher))]
      (if *usher-debug* (do (pprint usher) (read-line)))

      (if answer
        (:prog answer)
        (recur usher)))))
