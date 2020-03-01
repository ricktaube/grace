;
;; Defining Functions
;

; Use the 'define' command to create a new function, either by
; declaring a variable to hold the new function

(define foo (lambda (a b) (list a b)))

; or by using define's functional syntax consisting of the new
; function's 'calling signature; (a list of the name of the function
; followed by its input parameters) followed by the body of the
; function

(define (foo a b) (list a b)))

; Here is an example of a function definition using a signature. The
; signature declares the function's (pentatonic) followed by one input
; parameter 'k'. The body of the function contains a single statement,
; a call to the transpose function to compute the the scale the new
; function returns.

(define (pentatonic k)
  (transpose '(0 2 4 7 9) k))

; After defining the function you can call it with various key numbers
; as input

(pentatonic 60)

(pentatonic (between 60 72))

;
;; Predicate functions and conditional statements
;

; A 'predicate' is a function that returns true or false based on a
; test of its argument(s). Preicates often use what are called
; 'conditional statements' to accomplish their tasks.  The 'if then
; else' statement is the most flexble conditional: it consists of
; three parts: a test is first evaluated, if the test is true (not #f)
; then a 'then' statement is executed, otherwise an optional 'else'
; statement is executed. Here is the definition of a predicate
; function that returns true if its input key number is a black key,
; otherwise it returns false. Notice that the 'true' value that the
; function returns is the pitch class of the black key rather than
; boolean true. Since boolean logic says that any value that is not #f
; must, by definition, be true, the true value can be anything that is
; not stricly #f.  This allows our function to return useful
; information (the pitch class) when the key in question is actually a
; black key:

(define (black-key? k)
  (let ((p (pitch-class k)))
    (if (member p '(1 3 6 8 10))
        p
        #f)))

(let ((k (random 128)))
  (print "key: " k " is black: " (black-key? k))
  )

;
;; Keyword parameters
;

; Use s7's define* command to create a function that supports keyword
; parameters.  Keyword parameters are (optional) parameters that have
; default values so they are not required in the function call unless
; they differ from the default. If you dont explicity declare a
; default value when you define the function parameter its value will
; be initilaize to #f. Keyword parameters differ from required
; parameters in that they be specified by name as well as by
; position. This next example defines a function with four keyword
; parameters

(define* (triad (root 0) (3rd 4) (5th 7) (inv 0))
  (if (= inv 0) 
      (list root 3rd 5th)
      (if (= inv 1)
          (list 3rd 5th (+ root 12))
          (if (= inv 2) 
              (list 5th (+ root 12) (+ 3rd 12))
              #f))))

; No inputs returns the default triad

(triad)

; Diminished triad

(triad 0 3 6)

; The default triad in 1st inversion

(triad :inv 1)

; Second inversion minor triad

(triad :inv 2 :3rd 3)

