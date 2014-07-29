Usher
=====
Recursive program synthesis from example. Clojure implementation of [paper](http://research.microsoft.com/en-us/um/people/sumitg/pubs/cav13.pdf).

What it can do
--------------
It can generate programs and even recursive programs.  
It's still in alpha status. But here is what you already can do:

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
Which is a recursively defined `length` program generated with the following input:
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
Or we can generate Fibonacci program the same way:
```clojure
user> (magic-fib)
if i <= 1
  then i
  else (+ (fib dec i) (fib dec dec i))
```
And that is pretty cool.  
Result obtained from following examples and components:
```clojure
(run
   [0 1 2 3 4 5]
   [0 1 1 2 3 5]
   [{:fn dec       :ar 1}
    {:fn #(<= % 1) :ar 1}
    {:fn +         :ar 2}])
```

More examples in the [magic.clj](https://github.com/Oregu/usher/blob/master/src/usher/magic.clj) file.

To do
-----
- [ ] Optimisations (fibonacci take 5 sec to synthesize, while authors report 0.12 sec).
- [ ] Synthesize programs with arity > 1. (To generate append program.)
- [ ] More examples (append).

Research
--------
- [ ] Will it be able to support higher-order functions?
