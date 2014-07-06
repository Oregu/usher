(ns usher.core)

(defn init [in out]
  {:syn   {#(in) 0},      ; Collection of synthesized programs.
                          ;; (bipartite) Goal graph:
   :graph {:goals [out],  ; Goals.
           :resolvers [], ; Resolvers connecting goals to subgoals.
           :egdes [],     ; Edges connecting goals and resolvers.
           :root out},    ; Root, a result we should produce.
                          ;; Set of examples:
   :ex    [in out]})      ; Input and output vectors.

(def addp conj)

(defn synth [component programs]
  "Synthesizes new program by applying components to existing programs."
  component)

(defn forward [size syn comps]
  (reduce
    (fn [syn prog]
      (if (= (val prog) size)
        (addp syn (synth prog syn))
        syn))
    syn
    comps))

(defn split [usher]
  "SplitGoal rule, adds more resolvers to the goal graph."
  usher)

(defn wrap-p [p]
  (fn [& args]
    (let [[callee arity] p]
      (if (pos? arity)
        (try (apply callee args)
          (catch Throwable t :err))
        (callee)))))

(defn evalp [p in]
  "Evaluates program p given inputs vector in. Returns :err on error."
  (vec (map (wrap-p p) in)))

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (let [usher (init in out)
        syn   (:syn init)
        fwd   (forward 0 syn comps)
        fwd2  (forward 1 fwd comps)
        split (split usher)]
    (map #(evalp % in) fwd2)))


(defn zero [] 0)

(defn run-test-run []
  (run
    [[] [3] [1 2]] ; input
    [0 1 2]        ; output
    {zero   0,     ; components with arity a(c)
     empty? 1,
     inc    1,
     first  1,
     rest   1}))
