(ns usher.core)

(defn init [in out]
  {:syn   {identity 1},   ; Collection of synthesized programs.
                          ;; (bipartite) Goal graph:
   :graph {:goals [out],  ; Goals.
           :resolvers [], ; Resolvers connecting goals to subgoals.
           :egdes [],     ; Edges connecting goals and resolvers.
           :root out},    ; Root, a result we should produce.
                          ;; Set of examples:
   :ex    [in out]})      ; Input and output vectors.

(def addp conj)

(defn synth [component programs]
  "Synthesizes new program by applying components to existing programs"
  component)

(defn forward [syn comps]
  (let [ps (map #(synth % syn) comps)]
    (addp syn ps)))

(defn wrap-err [f]
  (fn [& args]
    (try (apply f args)
      (catch Throwable t :err))))

(defn evalp [p in]
  "Evaluates program p given inputs vector in. Returns :err on error."
  (vec (map (wrap-err (key p)) in)))

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (let [init (init in out)
        syn  (:syn init)
        fwd  (forward syn comps)]
    (map #(evalp % in) fwd)))

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
