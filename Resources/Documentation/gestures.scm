;
;; Building larger gestures from smaller ones
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; This file demonstrates how small bits of code can serve as motives
; (building blocks) for generating larger structures.

; Motive1 generates three notes in random order but always contains a
; whole step and minor seventh

(define (motive1 octave range chan)
  (process with pitches = (make-heap '(6 4 14))
           and amps = (make-heap '(.75 .5 .5))
           and offset = (ran range)
           repeat 3
           do
           (mp:midi :key (+ (next pitches) (* octave 12) offset)
                    :amp (next amps) :chan chan :dur .1)
           (wait .2)
           ))

; Listen to motive1 several times

(sprout (motive1 5 0 0))

; Try motive1 within a range several times 

(sprout (motive1 5 4 0))

; Motive2 generates a repeated note with one of the pair always
; accented

(define (motive2 octave range chan)
  (process with amps = (make-heap '(.75 .5 .5))
           and rhys = (make-heap '(.2 .2 .4))
           and offset = (ran range)
           repeat 3
           do
           (mp:midi :key (+ 0 (* octave 12) offset) 
                    :amp (next amps) :chan chan :dur .1)
           (wait (next rhys))
           ))

; Listen to motive2 several times

(sprout (motive2 5 0 1))

; Try motive2 within a range

(sprout (motive2 5 5 1))

; Make a gesture that chooses between motives two seconds apart

(define (gesture1 numtimes o chan)
  (process repeat numtimes
           do
           (if (odds o)
               (sprout (motive1 5 0 chan))
               (sprout (motive2 6 0 chan)))
           (wait 2)))

; Listen to gesture1, each sprout creates 10 motive

(sprout (gesture1 10 .5 0))

; The same but allow transpositions

(define (gesture2 numtimes o range chan)
  (process repeat numtimes
           do
           (if (odds o)
               (sprout (motive1 5 range chan))
               (sprout (motive2 6 range chan)))
           (wait 2)))


(sprout (gesture2 10 .5 5 0))

; Define a function that quantizes rhythms

(define (qtime n maxn s e quant)
  ;; over maxn steps move from s to e by quan step size 
  (quantize (interp (/ n maxn) 0 s .5 e) quant))
  

(loop for i below 10
      do (print (qtime i 10 2 4 .25)))

(define (gesture3 numtimes o range chan hiwait lowwait)
  (process for i from 0 to numtimes
           do
           (if (odds o)
               (sprout (motive1 5 range chan))
               (sprout (motive2 5 range chan)))
           (wait (qtime i numtimes 2 .2 .2))
           ))

; Gesture3 makes smaller amounts of time between motives

(sprout (gesture3 20 .5 5 0 3 .2))

; Gesture4 is similar but chooses octaves and gradually prefers
; motive2 over motive1

(define (gesture4 numtimes lowoctave highoctave range
                  chan hiwait lowwait)
  (process for i from 0 to numtimes
           do
           (if (odds (qtime i numtimes 1.0 0.0 .01))
               (sprout (motive1 (between lowoctave highoctave) range chan))
               (sprout (motive2 (between lowoctave highoctave) range chan)))
           (wait (qtime i numtimes hiwait lowwait .2))
           ))

(sprout (gesture4 30 2 7 11 0 1.6 .2))

; Sprout three gestures in parallel and use Piano, Marimba and Harp to
; sounds to play them

(begin
  ; chans 0,1,2 assigned Piano (0), Marimba (12) and Harp (46)
  (mp:instruments 0 12 46) 
  (sprout (gesture4 60 2 7 11 0 1.0 .2))
  (sprout (gesture4 40 5 7 11 1 1.6 .2))
  (sprout (gesture4 34 3 6 11 2 2.0 .2))
  )

; Reset channels back to piano

(mp:instruments 0 0 0)
