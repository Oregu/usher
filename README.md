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
user> (use 'usher.magic)
user> (magic-length)
```
And the result you'll see:
```
if empty? i 
  then zero 
  else inc length rest i 
```
Which is a recursively defined length program generated with the following input:
```clojure
(run
   [[ ] [2] [1 2]]     ; input examples
   [ 0   1    2  ]     ; output examples
   [{:fn zero   :ar 0} ; components
    {:fn empty? :ar 1}
    {:fn inc    :ar 1}
    {:fn first  :ar 1}
    {:fn rest   :ar 1}}])
```

To do
-----
- [ ] Generate Fibonacci program.
