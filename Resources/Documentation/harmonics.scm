;
;; Composing with the harmonic series
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; The harmonic series is easy to work with: to convert a harmonic
; number into a frequency, multiply a fundamental (in hertz)
; by the harmonic number. this hertz value can then be converted into
; keys or notes

(let ((fund 110)
      (harm1 1)
      (harm2 16))
  (loop for harm from harm1 to harm2
        for freq = (* fund harm)
        for keyn = (keynum freq)
        for name = (note keyn)
        do
        (print "harm=" harm " freq=" freq " key=" keyn " note=" name)
        ))

; Harmonics gesture. lets use this idea to create a four voice texture
; that plays the harmonics starting with each new prime number in the
; series:

(define (harmline n beat harm1 harm2)
  (let ((fund (hertz n))
        (ryth (* beat 4)))
    (process for harm from harm1 to harm2
             for freq = (* fund harm)
             for knum = (keynum freq)
             do
             (mp:midi :dur ryth :key knum :chan harm1)
             (wait ryth)
             )))

(mp:tuning 12)

(let ((primes '(2 3 5 7 11)))
  (loop for p in primes
        do
        (sprout (harmline "c1" (/ 1  p) p (* p 4)))
        ))

; Define a function that converts a range of harmonic numbers into a
; list of key numbers based on a fundamental keynumber:

(define (keyharms fundkey harm1 harm2)
  (keynum (loop with f = (hertz fundkey)
                for h from harm1 to harm2
                collect (* f (/ h harm1)))))

(print (keyharms 60 1 8))

(print (keyharms 60 8 16))

(print (note (keyharms 60 8 16)))

; keyharms2 does the same but for a list of harmonic numbers

(define (keyharms2 fundkey harms)
  (keynum (loop with f = (hertz fundkey)
                for harm1 = (first harms)
                for h in harms
                collect (* f  (/ h harm1)))))

(print (keyharms2 60 '(17 19 23 27 31 34)))

; We can turn a list of keynumbers it into a scale by repeating it
; over differnet octaves

(define (to-scale keys numoct)
  (loop for i below numoct
        append (plus keys (* 12 i))))

(print (to-scale (keyharms 60 8 16) 3))

; TODO: design a process that plays the series in some musica way

;
;; Exponentially stretched harmonics Etude (Michael Klingbeil)
;

(define (distort-harmonics fund distort)
  (loop for h from 1 below (floor (/ 25.0 distort))
        if (odds (* 0.9 distort))
        collect (keynum (* fund (expt h distort)))))

(print (note (distort-harmonics 110 .5)))

(define (arpeggiate-exprhy keynums time rate
                           midpoint-frac amplow
                           amphi legato bass-legato
                           bass-cutoff last-legato)
  (let* ((segn (length keynums))
         (last (- segn 1))
         (midpoint (round (* segn midpoint-frac)))
         ;; deltas below midpoint follow one curve, above another.
         (delta (append (segs midpoint
                              (* midpoint-frac time)
                              1 rate)
                        (segs (- segn midpoint)
                              (* (- midpoint-frac 1) time)
                              1
                              (/ 1 rate))))
         )
    (process for i from 0
             for k in keynums
             for d in delta
             for r = (if (< k bass-cutoff)
                         bass-legato
                         (if (= i last) (* last-legato d) (* legato d)))
             for a = (rescale i 0 last amphi amplow)
             do
             (mp:midi :key k :amp a :dur r)
             (wait d)
             )))

(define (arpa-harmonic nte dur gap)
  ;; spawn overlapping arpeggios with mean duration of dur and mean
  ;; gap between arpeggio starts of gap seconds. each arpeggio is
  ;; upward with the general direction of arpeggio starting notes
  ;; moving downward
  (let ((fund (hertz nte)))
    (process for distort from 0.7 below 1.05 by 0.05
             for notes = (distort-harmonics fund distort)
             do
             (sprout (arpeggiate-exprhy notes
                                        (* (vary dur 0.1) distort)
                                        (between 4.0 0.25)
                                        (between 0.3 0.6)
                                        0.3
                                        0.8
                                        (* dur distort 0.7)
                                        2.0
                                        59
                                        1.0))
             (wait (vary gap 0.4))
             )))

(mp:tuning 6)

(sprout (arpa-harmonic "g1" 7.0 5.0))

(sprout (list (arpa-harmonic "g1" 7.0 5.0)
              (arpa-harmonic "g1" 7.0 5.0)))

