(ns usher.core)

(defn init [in out]
  {:syn   [[[identity 1]]], ; Collection of synthesized programs.
                            ;; (bipartite) Goal graph:
   :graph {:goals  [out],   ; Goals (programs' valuations).
           :resolvers [],   ; Resolvers connecting goals to subgoals.
           :edges     [],   ; Edges connecting goals and resolvers.
           :root    out},   ; Root, a result we should produce.
                            ;; Set of examples:
   :ex    [in out]})        ; Input and output vectors.

(defn combine [v1 v2]
  "Utility. Combine two vectors."
  (if (empty? v2)
    v1
    (apply conj v1 v2)))

(defn gen-p [c p]
  "Generate new program with component (of size 1 for now)
   and existing program."
  (vec (cons c p)))

(defn synth [size component programs]
  "Synthesizes new program by applying components of given size
   to existing programs."
  (if (pos? size)
    ; TODO component should apply to existing goals
    ; TODO and generate final programs by graph walking.
    (mapv #(gen-p component %) programs)
    [[component]]))

(defn forward [size ps comps]
  (reduce
    (fn [syn c]
      (if (= (second c) size)
        (combine syn (synth size c ps))
        syn))
    ps
    comps))

(defn goal-intersect [g1 g2]
  "Returns boolean vector with truth in places of intersected positions.
  If no interse found, returns nil."
  (let [inter (mapv = g1 g2)]
    (if (some true? inter) inter)))

(defn g-intersects [g goals]
  "Returns bool vector of intersections between goal g and each of the goals.
   (With g.)"
  (reduce #(if-let [inter (goal-intersect g %2)]
            (conj %1 [inter g])
            %1)
          [] goals))

(defn g-conds [goals]
  "Produces _arbitrary_ cond goals."
  (let [upto (dec (count goals))]
    (loop [conds [] ind 0]
      (if (> ind upto)
        conds
        (recur
          (combine conds (g-intersects (goals ind) (subvec goals (inc ind))))
          (inc ind))))))

(defn g-then-else [cond+g]
  "For a cond goal vector build bthen and belse goals.
   Return [condg bthen belse]."
  (let [[cnd g] cond+g
        bthen (map-indexed (fn [ind itm] (if (true?  itm) (g ind) :?)) cnd)
        belse (map-indexed (fn [ind itm] (if (false? itm) (g ind) :?)) cnd)]
    [cnd (vec bthen) (vec belse)]))

(defn split [graph]
  "SplitGoal rule, adds more resolvers to the goal graph."
  (let [n (count (:root graph))
        gconds (g-conds (:goals graph))
        ; TODO do foreach then-else! Can be empty
        ifgoals (first (map g-then-else gconds))
        rslvr (keyword (str "r" (count (:resolvers graph))))]
    (-> graph
      ; Add goals: gcond, bthen, belse
      (update-in [:goals] #(conj % (ifgoals 0)))
      (update-in [:goals] #(conj % (ifgoals 1)))
      (update-in [:goals] #(conj % (ifgoals 2)))
      ; Add fresh resolver
      (update-in [:resolvers] #(conj %1 rslvr))
      ; Add 4 edges: (r, gdond), (gdond, r), (bthen, r), (belse, r)
      (update-in [:edges] #(conj %1 [rslvr (ifgoals 0)]))
      ; TODO here should indexes of goals and resolvers
      (update-in [:edges] #(conj %1 [(ifgoals 0) rslvr]))
      (update-in [:edges] #(conj %1 [(ifgoals 1) rslvr]))
      (update-in [:edges] #(conj %1 [(ifgoals 2) rslvr])))))

; TODO do saturation
(defn oracle [val ind in out]
  ; TODO well-defined relation, condition is bad too
  (if (and (> (count (in 2)) (count val)) (not= (in ind) val))
    (reduce
      #(if (= (first %2) val)
        (second %2)
        %1)
      :noval
      (map vector in out))
    :err))

(defn wrap-p [p in out]
  (fn [arg ind]
    (try
      (reduce #(if (pos? (second %2))
                (if (= (first %2) :self)
                  (oracle %1 ind in out)
                  ((first %2) %1))
                ((first %2)))
        arg
        (reverse p)) ; f(g(h(args))) or f(g(h))
      (catch Throwable t :err))))

(defn eval-p [p in out]
  "Evaluates program p given inputs vector in. Returns :err on error."
  (vec (map-indexed #((wrap-p p in out) %2 %1) in)))

(defn equal-g [g1 g2]
  (every?
    #(or
      (= (first %1) (second %1))
      (some (partial = :?) [(first %1) (second %1)]))
    (map list g1 g2)))

(defn match-g [ps g in out]
  "Find programs in ps which match goal g on input in."
  (some #(equal-g g (eval-p % in out)) ps))

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (let [usher (init in out)
        syn   (:syn usher)
        fwd1  (forward 0 syn comps)
        usher (assoc usher :syn fwd1)
        gs    (map #(eval-p % in out) fwd1)
        usher (update-in usher [:graph :goals] #(apply conj % gs))
        graph (split (:graph usher))
        usher (assoc usher :graph graph)
        fwd2  (forward 1 fwd1 comps)
        usher (assoc usher :syn fwd2)
        gs2   (map #(eval-p % in out) fwd2)
        ; After we generated more goals,
        ; we searching for programs
        ; that evaluate to searching goals
        ; gsat fill find already satisfied goals [true false false] should
        gsat  (filter #(match-g fwd2 % in out) (:goals graph))
        fwd3  (forward 1 fwd2 comps)
        gs3   (filter (fn [r] (some #(not= :err %) r)) (map #(eval-p % in out) fwd3))
        gsat2 (filter #(match-g fwd3 % in out) (:goals graph))
        fwd4  (forward 1 fwd3 comps)
        gs4   (filter (fn [r] (some #(not= :err %) r)) (map #(eval-p % in out) fwd4))
        gsat3 (filter #(match-g fwd4 % in out) (:goals graph))]
    [gsat gsat3]))


(defn zero [] 0)

(defn do-magic []
  (run
    [[] [2] [1 2]] ; input
    [0 1 2]        ; output
    [[zero   0]    ; components with arity a(c)
     [empty? 1]
     [inc    1]
     [first  1]
     [rest   1]
     [:self  1]])) ; TODO don't pass, should be internal
