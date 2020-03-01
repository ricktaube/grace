(definstrument (zn time dur freq amp length1 length2 feedforward)
  ;; notches are spaced at srate/len, feedforward sets depth thereof
  ;; so sweep of len from 20 to 100 sweeps the notches down from 1000 Hz to ca 200 Hz 
  ;; so we hear our downward glissando beneath the pulses.
  (let ((beg (seconds->samples time))
	 (end (seconds->samples (+ time dur)))
	 (s (make-pulse-train freq amp))
	 (d0 (make-notch :size length1 :max-size (+ 1 (max length1 length2)) :scaler feedforward))
	 (zenv (make-env '(0 0 1 1) :scaler (- length2 length1) :duration dur)))
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (notch d0 (pulse-train s) (env zenv))))))

;;(with-sound () (zn 0 1 100 .1 20 100 .995) (zn 1.5 1 100 .1 100 20 .995))

