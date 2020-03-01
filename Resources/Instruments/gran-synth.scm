(definstrument (gran-synth start-time duration audio-freq grain-dur grain-interval amp)
  (let ((grain-size (ceiling (* (max grain-dur grain-interval) *clm-srate*))))
    (let ((beg (seconds->samples start-time))
	  (end (seconds->samples (+ start-time duration)))
	  (grain-env (make-env '(0 0 25 1 75 1 100 0) :duration grain-dur))
	  (carrier (make-oscil audio-freq))
	  (grains (make-wave-train :size grain-size :frequency (/ 1.0 grain-interval))))
      (let ((grain (mus-data grains)))
	(do ((i 0 (+ i 1)))
	    ((= i grain-size))
	  (set! (grain i) (* (env grain-env) (oscil carrier)))))
      (do ((i beg (+ i 1)))
	  ((= i end))
	(outa i (* amp (wave-train grains)))))))

;;; (with-sound () (gran-synth 0 2 100 .0189 .02 .4))


