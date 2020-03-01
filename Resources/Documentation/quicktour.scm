;
;;  S7 Scheme Quick Tour
;

; To run examples put the cursor at the end of each expression and
; press Command-Return. Look in the console window for any output.

; S7 Scheme, by Bill Shottstaedt, is modern Scheme implementation that
; is partiuclary useful as a scripting language for audio
; applicataions. It provides an R5RS Scheme with numerous extensions,
; including compatabily with many Common Lisp functions. Grace uses S7
; as its base Lisp in which it implements Common Music functionality.

; Expressions

; An expression is anything that produces a value. Try evaluating
; these arithmetic expressions that involve integers, floats (numbers
; that include a decimal point) and ratios to see what they return:

123

(+ 55 33)

(= 44 33)

(* 60 2.0 1/3)

(* 7/3 5/16)

; Expressions don't have to be numeric, they can also involve lists
; (values inside {}), strings (words delimited by ""), the boolean
; values #t and #f (true and false), symbols (plain words) and
; keywords (symbols starting with ':') Here are all the common types
; of data collected in a surrounding list.

'(1 2.0 1/3 "hiho!" #t #f foo :foo (x y z) )

; Function Calls

; Computational work is accomplished by defining and calling
; functions. To call a function you type its name followed by whatever
; input values the function takes.  Evaluate these functions calls to
; see what sorts of expressions they return.

(list)

(list 1)

(list 1 2 (+ 3 4))

(between 50 100)

(even? 2)

(concat "Hi", "ho", "!")

; Function calls can be nested to any level. Interior function calls
; are evaluated before outer function calls.

(note (list (random 127) (random 127) (random 127)))

; Function calls can span more than a single line. In this case put
; the cursor after the last ')' and press Command-Return:

(concat '(Life is very) 
        (pick '(long short happy sad))
        )

; Some functions accept keyword arguments.  A keyword argument
; consists of a name starting with ':' (e.g. :foo ) followed by the
; argument's value. In this example the mp:midi function is called
; with two keyword arguments.

(mp:midi :key (between 60 90) :amp .8)

; Do not confuse keyword function arguments with keyword symbols, the
; latter are works that start with a colon, for example:

(pick :bach :berg :chopin)

;
;; Control Statements
;

; S7 Scheme provides a handful of control statements: 'if' 'begin' 'set' and
; 'loop'. All but 'set' has an 'end' tag.  To evaluate statements that
; terminate with an 'end', put your cursor just after the 'end' and
; press Command-Return.

; The 'if' statement tests a predicate expression to determine if its
; 'then' clause or its (optional) 'else' clause should be evaluated.

(if (odds .5)
    (list 1 2 3)
    (list -1 -2 -3))

; Use a 'begin' to execute more than a single "then" and "else" expression.

(if (odds .5)
    (begin (print "then clause is the winner!")
           (list 1 2 3))
    (begin (print "else clause is the winner!")
           (list 1 2 3))
    )

; The 'begin' statement sequences expressions, the last expression is
; returned as the value of the entire begin.

(begin
  (print "in begin!")
  (list 1 2 3))

; the let and let* statements are similar to begin but allow local
; variables to be bound as well.

(let* ((a 4)
       (b (+ a (random 12))))
  (print "in let, a=" a " b=" b)
  (list a b))


; the 'set!' statement lets you (re)assign values to existing variables

(let ((a 5))
  (set! a (random 100))
  a)

; The 'loop' statement defines an iteration. it is very similar to
; Common Lisp's loop macro

(loop repeat 10
      do 
      (print (random 100)))

(loop for i below 10
      do
      (print i))

(loop repeat 5
      for x = (random 200)
      do
      (print x " squared is " (expt x 2)))

; the 'finally' clause can be used to return a value

(loop with l = '()
      for i from 1 to 10
      do
      (set! l (append l (list i)))
      finally (return l))

; this kind of thing is more easily accomplished using the 'collect'
; operator

(loop for i from 1 to 10
      collect i)

; you can also append

(loop repeat 10
      for x = 0 then (drunk x 3)
      for y from 100 by 10
      append (list x y))

;
;; Definitions
;

; The define statement defines variables that are "global" in the
; environment in which they are defined

(define bar 33)

(list bar (* bar bar))

; The define statement can also create functions

(define (foo )
  (list 1 2 3))

(foo )

(define (foo a b)
  (list a b ))

(foo 100 200)

; Use let to declare local variables inside a function

(define (foo a b)
  (let ((c (list a b))
        (d 88))
    (list a b c d)))

(foo 100 200)

; define* creates functions with optkey parameters:

(define* (foo d (e 4))
  (list d e))

(foo)

(foo 2 3)

(foo 1)

(foo :e 99 :d -99)

; The 'process' statement defines an iterative function that blends
; features from functions and loops into a single entity:

(define (simp num rate)
  (process repeat num
           for k = (between 40 90)
           do
           (mp:midi :key k)
           (wait rate)
           ))

; Once you define a process you call 'sprout' to start it running:

(sprout (simp 20 .2))

; Here is the a little process that plays a cyclic pattern of notes
; over and over again until its endtime is reached

(define (piano-phase endtime keys rate)
  (process with pat = (make-cycle keys)
           while (< (elapsed) endtime)
           do
           (mp:midi :key (next pat) :dur rate)
           (wait rate)))

; This plays two copies of the process in parallel and in real time
; out your midi port (see reich.sal) for more inforamtion.)

(let ((keys (keynum '(e4 fs4 b4 cs5 d5 fs4 e4 cs5 b4 fs4 d5 cs5)))
      (stoptime 20))
  (sprout (list (piano-phase stoptime keys .167)
                (piano-phase stoptime keys .170)))
  )

