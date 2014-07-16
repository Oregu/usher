Usher
=====
Recursive program synthesis from example.  

Clojure implementation of [paper](http://research.microsoft.com/en-us/um/people/sumitg/pubs/cav13.pdf).

What it can do
--------------
It can generate length recursive function within 15 seconds. Very alpha.  
Consider not working.

Run it with
```clojure
lein repl
user> (use 'usher.core)
user> (do-magic)
if empty? i
  then zero
  else inc self rest i
```

To do
-----
- [ ] Generate Fibonacci program.
