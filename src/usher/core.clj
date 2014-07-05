(ns usher.core)

(defn init [in out]
  {:syn   {`identity 1},
   :graph {:goals [out],
           :resolvers [],
           :egdes [],
           :root out},
   :ex    [in out]})

(def addp conj)

(defn synth [component programs]
  "Synthesizes new program by applying components to existing programs"
  #_(addp component programs)
  component)

(defn forward [syn comps]
  (let [c (first comps)
        p (synth c syn)]
    #_(addp p syn)
    (apply hash-map p)))

(defn evalp [p in]
  "Evaluates program p given inputs vector in"
  (map (eval (key p)) in))

(defn run [in out comps]
  {:pre [(= (count in) (count out))]}
  (let [init (init in out)
        fwd (forward (:syn init) comps)]
    (map #(evalp % in) fwd)))

(defn zero [] 0)

(defn run-test-run []
  (run
    [[] [3] [1 2]] ; input
    [0 1 2]        ; output
    {`zero   0,    ; components with arity a(c)
     `empty? 1,
     `inc    1,
     `first  1,
     `rest   1}))
