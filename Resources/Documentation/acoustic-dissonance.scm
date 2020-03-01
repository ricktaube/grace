;;;  July 29, 1998
;;;
;;;  Main function is 'acoustic-dissonance.'  See documentation for
;;;  the function for examples of use. This program is based on the
;;;  work of Hutchinson & Knopoff (1978) as slightly modified by
;;;  Richard Parncutt. You are welcome to use it in any way you like
;;;  but I take no responsibility for any lame implementation choices
;;;  I may have made...
;;;
;;;					-Sean Ferguson
;;;					 McGill University
;;;					 Montreal, Canada
;;;					 ferguson@music.mcgill.ca

#|
The references are:

Hutchinson, William and Leon Knopoff. "The Acoustic Component of Western
Consonance." Interface, Vol. 7 (1978), pp. 1-29.

Hutchinson, William and Leon Knopoff. "The significance of the acoustic
component of dissonance in Western triads." Journal of Musicological
Research, Vol. 3, 1979, pp. 5-22

Thompson, William Forde and Richard Parncutt. "Perceptual Judgments of
Triads and Dyads: Assessment of a Psychoacoustic Model." Music Perception,
Spring 1997, Vol. 14, No. 3, 263-280.

Parncutt, Richard and Hans Strasburger. "Applying Psychoacoustics in
Composition: 'Harmonic' Progression of 'Nonharmonic' Sonorities." This is
in a relatively recent volume of Perspectives of New Music, but
unfortunately I don't have the exact year with me. It is after 1990, in
any case.

Parncutt, Richard. Harmony: a Psychoacoustical Approach. Springer-Verlag
1989

Danner, Gregory. "The Use of Acoustic Measures of Dissonance to
Characterize Pitch-Class Sets." Music Perception Vol. 3, No. 1, 103-122,
Fall 1985.

Balsach, Llorenc. "Application of Virtual Pitch Theory in Music Analysis.
Journal of New Music Research, Vol. 26 (1997), pp. 244-265.

It is probably best to start with the Hutchinson and Knopoff and then just
muck around after that. The Parncutt book is also very good.
|#

;;There *must* be a better way to do this...
(define (round-off number-of-places floating-point-number)
  ;; Rounds off a floating-point number to the given number of places.
  ;;   (round-off 2 3.234264) -> 3.23
  ;;   (round-off 4 3.234264) -> 3.2343"
  (let ((x (expt 10.0 number-of-places)))
    (/ (round (* x floating-point-number)) x)))

; (round-off 3 12.345678)

(define (midi->pitcat midi-pitch)
  ;; Converts MIDI pitch numbers (middle c = 60) to Parncutt's pitch
  ;; categories (middle c = 48)
  (- midi-pitch 12))

; (midi->pitcat 60)

;;Taken from Parncutt...

(define (pitch->Hz midi-pitch)
  ;; Translates a MIDI pitch number (middle c = 60) into a frequency
  ;; given in Hz. So (pitch->Hz 69) returns 440 Hz."
  (* 440 (expt 2.0 (/ (- (midi->pitcat midi-pitch) 57) 12))))

; (pitch->Hz 69)
; (pitch->Hz 60)

(define (mean-freq f1 f2)
  ;; Returns the mean frequency of two pure tones in Hz.  Equals 1/2(f1 + f2).
  (/ (+ f1 f2) 2.0))

; (mean-freq 440 100)

(define (critical-bandwidth f)
  ;; Returns the critical bandwidth in Hz for the given frequency,
  ;; also in Hz (a4 = 69 = 440 Hz)."
  (* 1.72 (expt f .65)))

; (critical-bandwidth 440)
; (critical-bandwidth (pitch->Hz 30))

(define (cbw-interval f1 f2)
  ;; Gives the interval between two partials in units 
  ;; of the critical bandwidth. Frequencies should be given
  ;; in Hz.
  (/ (abs (- f2 f1))
     (critical-bandwidth (mean-freq f1 f2)))) 

; (cbw-interval 440 100)
; (cbw-interval 440 (pitch->Hz 60))

(define (standard-curve cbw-int)
  ;; Parncutt's function for g(y) of H&K (p. 4). Gives the dissonance
  ;; weighting factor of the frequency difference of two pure tones in 
  ;; units of the critical bandwidth."
  (if (> cbw-int 1.2) ;if critical bandwidth interval > 1.2, *no* roughness
    0
    (let ((ratio (/ cbw-int .25))) ;.25 is interval for max roughness
      (expt (* (* 2.7182818 ratio)
               (exp (* -1 ratio)))
            2))))

; (standard-curve (cbw-interval 440 400))
; (standard-curve (cbw-interval 440 500))

(define (pure-tone-dissonance f1 f2)
  ;; Gives the pure tone dissonance of two partials given in Hz without 
  ;; considering amplitude. That is, amp for both f1 and f2 = 1.
  (standard-curve (cbw-interval f1 f2)))

; (pure-tone-dissonance 440 400)
; (pure-tone-dissonance 440 500)

(define (harmonic-interval harmonic-number)
  ;; Gives the interval in semitones between fundamental and harmonic.
  (floor (+ (/ (* 12 (log harmonic-number)) (log 2)) .5)))

; (harmonic-interval 23)

(define (harmonic-series-pitch fundamental-pitch . no-of-harms)
  ;; Give harmonic series rounded off to chromatic pitches.
  (set! no-of-harms (if (null? no-of-harms) 10 (car no-of-harms)))
  (loop for x from 1 to no-of-harms
        collect (+ (harmonic-interval x) fundamental-pitch)))

; (harmonic-series-pitch 36 11)

(define (harmonic-series-frequency fundamental-pitch . no-of-harms )
  ;; Gives the harmonic series in frequency rounded off to chromatic pitches.
  ;; Fundamental pitch is given as MIDI note number (middle c = 60).
  (set! no-of-harms (if (null? no-of-harms) 10 (car no-of-harms)))
  (map pitch->Hz (harmonic-series-pitch fundamental-pitch no-of-harms)))

; (harmonic-series-frequency 36 11)

(define (sum-amplitudes amp1 amp2)
  ;; Returns the summed amplitudes of arguments.  Amplitudes are 
  ;; given as fractions of harmonic numbers (1/n). For example, 
  ;; the amplitude of harmonic 3 is 1/3.
  (sqrt (+ (* amp1 amp1) (* amp2 amp2))))

; (sum-amplitudes .2 .1)

(define (get-harm-amps harm-series)
  (loop for harm in harm-series
        for x = 1 then (+ x 1)
        collect (cons harm (/ 1 x))))

; (get-harm-amps '(1 2 3 4 5 6 7))

(define (get-amps-of-overlaps overlaps spect1+amps spect2+amps)
  ;; To be called by merge-spectrums only.
  (do ((walk overlaps (cdr walk))
       (result (list )))
      ((null? walk) result)
    (let ((x (car walk)))
      (set! result (cons (cons x (sum-amplitudes
                                  (cdr (assoc x spect1+amps))
                                  (cdr (assoc x spect2+amps))))
                         result)))))

(define (remove-overlaps orig final pool)
  (cond ((null? orig) final)
        ((member (caar orig) pool)
         (remove-overlaps (cdr orig) final pool))
        (else
         (remove-overlaps (cdr orig)
                          (cons (car orig) final)
                          pool))))
              
; (list-intersection '() '())
; (list-intersection '(1.0) '())
; (list-intersection '(0.0 1.0 2.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0 4.0) '(0.0 1.0 2.0))
; (list-intersection '(1.0 2.0 4.0) '(0.1 1.0 2.1))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.1 a b) (1.0 a b) (2.1 a b)) :key car)
; (list-intersection '((1.0 a b)) '(()))
; (list-intersection '((0.0 a b) (1.0 a b) (2.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.0 a b) (1.0 a b) (2.0 a b)))
; (list-intersection '((1.0 a b) (2.0 a b) (4.0 a b)) '((0.1 a b) (1.0 a b) (2.1 a b)) :getter car)
; (list-intersection '( (60 . 1) (72 . 1/2) (79 . 1/3) (84 . 1/4) (88 . 1/5) (91 . 1/6) (94 . 1/7) (96 . 1/8) (98 . 1/9) (100 . 1/10)) '( (61 . 1) (73 . 1/2) (80 . 1/3) (85 . 1/4) (89 . 1/5) (92 . 1/6) (95 . 1/7) (97 . 1/8) (99 . 1/9) (101 . 1/10)) )

;;This isn't that efficient, but it isn't really called very often.
(define (merge-spectrums fund-pitch1 . pitches)
  ;; Returns a list of dotted pairs with car = pitch of spectral
  ;; component (in MIDI numbers) and cdr = relative amplitude.
  ;; Components which occur more than once have their amplitudes
  ;; scaled appropriately.
  (if (not (null? pitches)) (set! pitches (car pitches)))
  (let ((result (get-harm-amps (harmonic-series-pitch fund-pitch1))))
    (if (null? pitches) 
        result
        (letrec ((rec1 (lambda (final current pchs)
                         (let* ((spect+amps (get-harm-amps (harmonic-series-pitch current)))
                                (overlaps (map car 
                                               (list-intersection final spect+amps :getter car)))
                                (overlap+amps (get-amps-of-overlaps overlaps final spect+amps))
                                (merged-spects+amps (append final spect+amps))
                                (gapped-spects+amps (remove-overlaps merged-spects+amps
                                                                     (list)
                                                                     overlaps))
                                (re-merged (sort (append overlap+amps
                                                         gapped-spects+amps)
                                                 (lambda (a b) (< (car a) (car b)))
                                                 )))
                           (cond ((null? pchs) re-merged)
                                 (else (rec1 re-merged
                                             (car pchs)
                                             (cdr pchs))))))))
          (rec1 result (car pitches) (cdr pitches))
          
          ))))

; (merge-spectrums 60 '(61 62 73))
; (merge-spectrums 62 '(67 72 77 82))

(define (not-too-big-p x y)
  ;; Tests to see if two MIDI pitches are small enough to
  ;; bother testing for pure-tone-dissonance.  A time-
  ;; saving function for 'acoustic-dissonance.'"
  (let ((diff (abs (- x y)))
        (lower (min x y)))
    (or (and (< lower 24) (< diff 11)) 
        (and (< lower 36) (< diff 10))
        (and (< lower 48) (< diff 8))
        (and (< lower 60) (< diff 7))
        (and (< lower 72) (< diff 5))
        (and (< lower 86) (< diff 4))
        (and (< lower 106) (< diff 3))
        (and (> lower 105) (< diff 2)))))
                              
(define (diss-numerator pch-amp1 pch-amp2)
  ;; Pitches must be given as MIDI notes.
  (let ((pch1 (car pch-amp1))
        (pch2 (car pch-amp2))
        (amp1 (cdr pch-amp1))
        (amp2 (cdr pch-amp2)))
    ;;This conditional statement increases speed by up to 5 times or more!
    (if (not (not-too-big-p pch1 pch2)) ;Check to see if worth computing,
        0 				;if not, then return 0.
        (* (* amp1 amp2) ;Numerator 	;Otherwise, calculate numerator
           (pure-tone-dissonance (pitch->Hz pch1)
                                 (pitch->Hz pch2)))))) ;Numerator

(define (acoustic-dissonance pitches) 
  ;; Given a list of one or more midi pitches, returns the
  ;; acoustic-dissonance, or 'roughness.' For example:
  ;; ? (acoustic-dissonance '(60 61 62 73))
  ;; 0.7287
  ;; ? (acoustic-dissonance '(60))
  ;; 0.0012
  ;; ? (acoustic-dissonance '(47 49 53))
  ;; 0.3846
  ;; ? (acoustic-dissonance '(62 67 72 77 82))
  ;; 0.1592
  (let* ((combined-spectrum (merge-spectrums (first pitches) (rest pitches)))
         ;;denominator is total amplitude of spectrum
         (denominator (apply + (map (lambda (x) (* x x)) 
                                    (map cdr combined-spectrum))))
         ;; do* variables
         (comb-spect combined-spectrum )
         (lowest (car combined-spectrum) )
         (higher (cdr combined-spectrum) )
         (interim-list (map (lambda (x) (diss-numerator lowest x)) higher) )
         (result interim-list ) )
    (do ()
        ((null? (rest comb-spect))
         (round-off 4 (/ (apply + result) denominator)))    ;add up all numerators and then divide
      (set! comb-spect (cdr comb-spect))
      (set! lowest (car comb-spect))
      (set! higher (cdr comb-spect))
      (set! interim-list (map (lambda (x) (diss-numerator lowest x)) higher))
      (set! result (append interim-list result))))
  )

;(define (intersection list1 list2 . getter)
;  (set! getter (if (null? getter) #f (car getter)))
;  (if (not getter)
;      (apply append (loop for x in list1
;                          collect (loop for y in list2 if (equal? x y) collect y)))
;      (let ((xx #f)
;            (yy #f))
;        (apply append
;               (loop for x in list1
;                     do (set! xx (getter x))
;                     collect (loop for y in list2 do (set! yy (getter y)) if (equal? xx yy) collect y))))))

(define (acoustic-sort chords . cons-to-diss)
  (set! cons-to-diss
        (if (and (pair? cons-to-diss) (eq? (car cons-to-diss) #f))
            > <))
  (let ((rank (map (lambda (x) (cons (acoustic-dissonance x) x)) chords)))
    (map cdr (sort rank (lambda (a b) (cons-to-diss (car a) (car b)))))))





