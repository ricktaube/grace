;
;; Ghosts
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Ghosts gets its name from the fact that a short, randomly generated
; 12 note melody serves as the basis for computing other bits of
; dependent "ghost" structure that echo the melody in different
; musical ways at much larger temporal levels. Ghosts generates three
; accompanying gestures to go along with the short melody

; 1. Define a high, temporally stretched version of the melody

(define (hitone knum dur)
  (process repeat 1 
           do (mp:midi :key (+ knum 24) :dur dur :amp .5)
           ))

; Listen to hitone a few times.

(sprout (hitone (between 60 67) 1))

; 2. Define a low percussive "thump" that accompanies low melodic
; tones

(define (thump knum)
  (process repeat 1
           do
           (mp:midi :key (- knum 18) :dur .05 :amp .4)
           (mp:midi :key (- knum 23) :dur .05 :amp .4)
           ))

; Listen to thump a few times.

(sprout (thump (between 60 67)))

; 3. Define a distant strum that accompanies the high stretched melody
; but at even larger time scales.

(define (strum knum rate)
  (process repeat 5
           for k from (+ 39 (modulo knum 13)) by 13
           do
           (mp:midi :key k :amp .3 :dur 10)
           (wait rate)))

; Listen to strum a few times.

(sprout (strum (between 60 67) .2))

; The ghosts process. Generates 12 notes and adds accompanying figures
; depending qualities of the main melody
	
(define (ghosts)
  (process repeat 12
           for here = (elapsed)
           for ahead = (* (+ here 1/2) 2)
           for main = (between 53 77)
           for high? = (>= main  65)
           for amp =  (if high? .6 .4)
           for rhy = (pick 1/4 1/2 3/4)
           do
           (mp:midi :key main :dur (+ rhy .2) :amp amp)
           (if high?
               (sprout (hitone main ahead) ahead)
               (sprout (strum main (/ rhy 4)) (* ahead 2)))
           (if (= rhy 3/4)
               (sprout (thump main) 1/2))
           (wait rhy)))

; Play a phrase of ghosts

(sprout (ghosts))


