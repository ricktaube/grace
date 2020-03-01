;
;; More Patterns
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Functional data in patterns.

; You can embed thunks (functions of zero arguments) inside patterns,
; when a thunk is encountered it will be called to produce the value
; to return from the pattern:

(define (ran1) (between 48 60))
(define (ran2) (between 60 72))
(define (ran3) (between 72 85))

(let ((pat (make-cycle (list ran1 ran2 ran3))))
  (next pat 30)
  )

; Creating patterns that mix functions or subpatterns with constant
; data can be a cumbersome. for example if you want to create a cyclic
; pattern whose data included some symbols and a heap subpattern e.g
; {c b c <heap> d e f} you might do something like:

(let* ((sub (make-heap '(x y z)))
       (pat (make-cycle (concat '(a b c) sub '(d e f)))))
  (next pat 20)
  )

; Its much easier to use backquote ` and commas , to
; mix subpatterns and data, for example:

(let* ((sub (make-heap '(x y z)))
       (pat (make-cycle `(a b c , sub d e f))))
  (next pat 20)
  )

; Here is another example

(let ((pat (make-cycle `(c4 , (make-cycle '(ef4) :for (make-cycle '(1 2)))
                            , (make-cycle '(b4) :for (make-cycle '(3 2 0)))
                            d4))))
  (next pat 40)
  )

; How it works. Without , things inside a `() list are contants:

(let* ((sub (make-heap '(x y z)))
       (pat (make-cycle `(a b c sub d e f))))
  (next pat 20))

; Inside a `() the , evaluates the next thing when the list is created.
; thus, the thing will be replaced by its (evaluated) value. 
; Execute this block a few times to see that the second
; element can change each time

(let ((lis `(a ,(random 128) b)))
  lis)

; If you pass that list to pattern and then generate pattern data you
; will see that random number reappearing in the pattern:

(let* ((lis `(a , (random 128) c))
       (pat #f)
       (dat #f))
  (print "pattern list=" lis)
  (set! pat (make-cycle lis))
  (set! dat (next pat 30))
  (print "pattern data=" dat)
  )

;
;; Embedding expressions in patterns.
;

; You can also embed EXPRESSIONS inside patterns such that the
; expression will produce a new value each time next() reads it from
; the patter. To do this you must DELAY the evaluation of the
; expression when you create the list of data you pass to the pattern,
; either by making it the return value of a function of zero arguments
; or by using the handy 'promise' function:

(let* ((mylist `(a , (promise (between 30 90)) c))
       (mypat (make-cycle mylist)))
  (print "mylist=" mylist)
  (print "pattern data=" (next mypat 30))
  )
                 
; probability weights as a function of time. 

; lets use embedded expressions to create a weighting whose
; probailities change as a function of x. in this example as x goes
; from 0 to 1 wei goes from 0 to 10 according to env:

(let* ((len 50)
       (env '(0 1 1 10))
       (maxx (- len 1))
       (wei #f))
  (loop for i from 0 below len
        for x = (/ i  maxx)
        do
        (set! wei (interp x env))
        (print "weight=" wei)
        ))

; To use wei as a probablity weight inside a pattern we can use
; promise to delay the evaluation of wei and put that in a pattern
; element. in this pattern the probability of B increases from 0 to 10
; as x goes from 0 to 1.

(let* ((len 50)
       (env '(0 0 1 10))
       (maxx (- len 1))
       (wei #f))
  (loop with pat = (make-weighting `(a (b , (promise wei)) c) :for 1)
        for i from 0 below len
        for x = (/ i maxx)
        do
        (set! wei (interp x env))
        (print "item=" (next pat))
        ))

; Here is a musical rendition of this that pulls the notes CAGE out a
; a G-dorian backgroud:

(define (cage num envl off)
  (let* ((w 0)
         (pat1 (make-weighting `((g3 , (promise w))
                                 (a3 , (promise w))
                                 bf3
                                 (c4 , (promise w))
                                 d4
                                 (e4 , (promise w))
                                 f4
                                 (g4 , (promise w)))
                                ))
         (pat2 (make-weighting `(1/4
                                 1/2
                                 , (make-cycle 1/8 :for 2))
                               )))
    (process for i below num
             for k = (keynum (next pat1))
             for r = (in-tempo (next pat2) 60)
             do
             (set! w (interp (/ i num) envl))
             (mp:midi :dur (* r 1.25) :key (transpose k off))
             (wait r)
             )))
     
(sprout (list (cage 100 '(0 .5 1 6) -12)
              (cage 100 '(0 .5 1 6) 0)
              (cage 100 '(0 .5 1 6) 12)
              (cage 100 '(0 .5 1 6) 24)))

; The Graph pattern. A graph is a network of nodes, each node contains
; a 'value' (the item to return from the pattern), a 'link' to the
; node that should come next , and an identifier (a unique name for
; the node in the graph). Both the value and the link can be
; subpatterns . node identifiers default to increasing numbers from 1

; here is how to create a cycle of A B C as a graph (the first node is
; always followed by node 2, the second node by node 3, and node 3
; returns to node 1:

(define pat (make-graph '((A 2) (B 3) (C 1))))

(next pat 12)

; An 'alberti bass' figure uing the triad C E G

(define pat (make-graph `((c 3)
                          (e 3)
                          (g , (make-cycle '(2 1)))
                          )))

(next pat 24)

; A randomizing alberti figure:

(define pat (make-graph `((c 3)
                          (e 3)
                          (g , (make-weighting '(2 1)))
                          )))


(next pat 24)

; Extending the alberi idea to a graph of subpatters:

(define pat (make-graph `((, (make-cycle '(c e g)) 3)
                          (, (make-cycle '(f a c)) 3)
                          (, (make-cycle '(g b d)) , (make-weighting '(2 1)))
                          )))

(next pat 48)

; First order markov chant as a graph

(define pat (make-graph (list (list "c4" (make-weighting '(2 5)))
                              (list "d4" (make-weighting '(1 3)))
                              (list "ef4" (make-weighting '(2 4)))
                              (list "f4" (make-weighting '(3 5)))
                              (list (make-heap '("g4" "a4" "bf4" "c5"))
                                    (make-cycle '(1 2 3 4))))
                        ))

(next pat 80)

; Play it 

(define (play-pat reps patr rate)
  (process repeat reps 
           for n = (next patr)
           do 
           (if (not (rest? n))
               (mp:midi :key (keynum n) :dur (* rate 1.5)))
           (wait rate)
           ))

; Play chant in 6 8 or 12 note phrases separated by a rest

(let ((pat1 (make-cycle (list (make-graph (list (list "c4" (make-weighting '(2 5)))
                                                (list "d4" (make-weighting '(1 3)))
                                                (list "ef4" (make-weighting '(2 4)))
                                                (list "f4" (make-weighting '(3 5)))
                                                (list (make-heap '("g4" "a4" "bf4" "c5"))
                                                      (make-cycle '(1 2 3 4)))
                                                )
                                          (make-heap '(6 8 12))
                                         )
                              "r")
                         )))
  (sprout (play-pat 80 pat1 .2)))
