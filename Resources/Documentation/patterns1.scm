;
;; Patterns
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Patterns are objects that generate data in different orderings. The
; cycle pattern is the simplest pattern, it generates data in an
; endless loop from front to back. to use a pattern you first have to
; make it:

(define pat (make-cycle '(a b c d )))

; Once you have a pattern use the next function to read data from it.
; execute this statement a number of times to see what is printed

(next pat)

; Use note key or hertz to convert data into notes keynums or hertz
; values before you put them in pattern. Remember that octave numbers
; are sticky in note lists:

(define pat (make-cycle (note '(a4 b c5 d e))))

; Calling next with #t reads a whole "period" or "chunk" of elements
; from the pattern:

(next pat #t)

(define pat (make-cycle (keynum '(a4 b c5 d e))))

(next pat #t)

(define pat (make-cycle (hertz '(a4 b c5 d e))))

(next pat #t)

(define pat (make-cycle (note '(a b c5 d e))))

; For simple streams, if the second argument is a number then a list
; of that many elements are returned from the pattern

(next pat 3)

; The for: parameter lets you specify a "period size" for the stream
; when you create it

(define pat (make-cycle (note '(a4 b c5 d e)) :for 3))

; The period length of this pattern is different than the number of
; elements it contains.

(next pat #t)

; remember that for: is a named parameter so you can specify it
; positionally as well:

(define pat (make-cycle (note '(a4 b c5 d e)) 3))

(next pat #t)

; Important pattern rule: Constant data can be replaced by patterns of
; data. So we can replace the number 3 with a pattern of period
; lengths

(define pat (make-cycle '(a b c d) :for (make-cycle '(2 3 4 5))))

(loop repeat 8
      do (print (next pat #t)))

; Since we can replace constant data with patterns, we can create
; subpatterns of data. in this example we put a heap pattern inside a
; cycle pattern. A <heap> randomly shuffles its data.

(define pat (make-cycle (list (make-cycle '(a1 a2 a3))
                              (make-cycle '(b1 b2 b3))
                              (make-cycle '(c1 c2 c3)))))

(next pat #t)

(define pat (make-cycle (list (make-cycle '(a1 a2 a3) :for 1)
                              (make-cycle '(b1 b2 b3) :for 1)
                              (make-cycle '(c1 c2 c3) :for 1))))

(next pat #t)

; A heap pattern generates its data in random order by shuffling it each time it starts over:

(define pat (make-heap (note '(a4 b c5 d e))))

(next pat #t)

; TODO: create a cycle of heaps and a heap of cycles and listen to
; the difference

; You can stick a pattern at any point in the data specified to
; another pattern

(define pat (make-cycle (concat (make-heap '(a4 a5 a6) 1) '(b4 c5 d e))))
                  
(loop repeat 5
      do (print (next pat #t)))

                                    
; A palindrome generates items forwards and backwards

(define pat (make-palindrome '(a b c d e)))

(next pat #t)

; Use the elide argument to enable/disable direct repetition of the
; terminal values

(define pat (make-palindrome '(a b c d e) :elide #t))

(next pat #t)

; To elide just the beginning or the end of the pattern specify a list
; of two values:

(define pat (make-palindrome '(a b c d e) :elide '(#t #f)))

(next pat #t) 

(define pat (make-palindrome '(a b c d e) :elide '(#f #t)))

(next pat #t) 

; A line sticks on the last element in the pattern

(define pat (make-line '(a b c d e)))

(next pat #t)

(define pat (make-line (list 1 2 3 (make-heap '(a b c d)))))

(next pat #t)

; A weighting performs discrete random selection, by default all
; values have an equal likelyhood of being selected:

(define pat (make-weighting '(a b c d)))

(next pat #t)

; To provide more (or less) weight for a given item specify it as a
; list together with its weight: (item weight)

(define pat (make-weighting '((a 3) b c d)))

(next pat 20)

(define pat (make-weighting '((a 3) (b 10) c d)))

(next pat 20)

; A min value specifies how many times it must be directly selected

(define pat (make-weighting '(a (b 1 2) c d)))

(next pat 20)

; A max value specifies max times it might be directly selected

(define pat (make-weighting '(a (b 1 1 2) c d)))

(next pat 20)

;
;; Working with Patterns
;

; You can make functions to return them

(define (mypat notes)
  (make-cycle (list (make-heap notes)
                    (make-heap (transpose notes 12))
                    (make-heap (transpose notes -12))
                    )))

(define pat (mypat (note '(a4 b c5 d e))))

(next pat 20)

(define (playpat num notes rate)
  (process with zztop = (mypat notes)
           repeat num
           for n = (next zztop)
           do
           (mp:midi :dur (* rate 2) :key (keynum n) :amp .7)
           (wait rate)))

(sprout (playpat 30 (note '(a4 b c5 d e)) .2))
    
;
;; Designing patterns
;

; TODO: create mypat2 that is like mypat but it take a list of
; intervals and a list of transpostion offsets and return a cycle
; containing as many heaps as offsets with each heap containing the
; intervals tranposed to the offset

; TODO: design a pattern that randomly returns quarters, eighth s and
; triplets. if eights are selected then 2 must be returned, if
; triplets are picked then three must be returned. no more than 2
; quarters can be consecutively returned.


; Here is a little example that generates randomly constructed chords

(define (makechords )
  (make-cycle (list (make-heap (note '(c6 d ef f g a bf))
                               (make-weighting '(3 4 5)))
                    (make-heap (note '(c5 d ef f g a bf))
                               (make-weighting '(3 4 5)))
                    (make-heap (note '(c4 d ef f g a bf))
                               (make-weighting '(3 4 5))))
                 :for 1))

(define (makerhythms)
  (make-cycle '(1 1 1 .5)))

(define pat (makechords))

(next pat #t)

(define (playjazzchords num notepat rhypat tpo)
  (process repeat num
           for rhy = (in-tempo (next rhypat) tpo)
           for chd = (next notepat #t)
           do
           (loop for n in chd
                 do
                 (mp:midi :key (keynum n) :dur .1 :amp .8))
           (wait rhy)
           ))


(sprout (playjazzchords 30 (makechords) (makerhythms) 160))
