;
;; A la maniere de 'continuum' (Gyorgi Ligeti)
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; Generates a chromatic scale betwen low and high note choosing notes
; from the scale in random

(define (register rhy dur low high amp)
  (process with key-pat = (make-heap (scale (+ (- high low) 1) low 1))
           for keyn = (next key-pat)
           until (>= (elapsed) dur)
           do
           (mp:midi :key keyn :dur rhy :amp amp)
           (wait rhy)))

; play fast notes for 5 seconds between C4 and F#4 (keynums 60 and 66)

(sprout (register 0.1 5 60 66 .5))

(define (continuum tmp low-keys high-keys rhy-pat)
  (process for low in low-keys
           for high in high-keys
           for dur = (next rhy-pat)
           do
           (sprout (register tmp dur low high .4))
           (wait dur)))

; Set channel 0 to harpsichord 

(mp:instruments 6)

; Sprout a continuum

(sprout (continuum .075
                   '(60 59 58 57 56 55 54 53 52
                        53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
                        69 70 71 72 73 74 75 76 77 78
                        79 80 82 83 84 85 86 87 88 89)
                   '(62 63 64 65 66 67 68 69 70 
                        70 70 70 70 70 70 70 70 70 70 70 70 70 70 70 70
                        71 72 73 74 76 79 83 86 88 89
                        89 89 89 89 89 89 89 89 89 89)
                   (make-weighting '(.5 1 1.5 2 2.5))))
        
; Plot the two evelopes to see the interval between them

(let ((l1 '(60 59 58 57 56 55 54 53 52
               53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
               69 70 71 72 73 74 75 76 77 78
               79 80 82 83 84 85 86 87 88 89))
      (l2 '(62 63 64 65 66 67 68 69 70 
               70 70 70 70 70 70 70 70 70 70 70 70 70 70 70 70
               71 72 73 74 76 79 83 86 88 89
               89 89 89 89 89 89 89 89 89 89))
      (env1 (list))
      (env2 (list)))
  (loop for x from 1 for y in l1
        do (set! env1 (append env1 (list x y))))
  (loop for x from 1 for y in l2
        do (set! env2 (append env2 (list x y))))
  (plot env1 env2)
  )

