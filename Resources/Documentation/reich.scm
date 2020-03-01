;
;; Steve Reich's Piano Phase
;

; To run code put your cursor after each expression and press
; Command-Return then check the console window for any output.

(define (piano-phase endtime keys rate)
  (process with pat = (make-cycle keys)
           while (< (elapsed) endtime)
           do (mp:midi :key (next pat) :dur rate)
           (wait rate)))


; Play the example in real time out your midi port

(let ((keynums (keynum '(e4 fs4 b4 cs5 d5 fs4 e4 cs5 b4 fs4 d5 cs5)))
      (stoptime 20))
  (sprout (list (piano-phase stoptime keynums .167)
                (piano-phase stoptime keynums .170)))
  )

; You can generate output to a file as long as no real-time processes
; are currently running. This example will create a midifile called
; reich.mid in your current working directory.

(let ((keynums (keynum '(e4 fs4 b4 cs5 d5 fs4 e4 cs5 b4 fs4 d5 cs5)))
      (stoptime 20)
      (midifile "reich.mid"))
  (sprout (list (piano-phase stoptime keynums .167)
                (piano-phase stoptime keynums .170))
         midifile)
  )

(play "reich.mid")

