;;; interpolate between two waveforms (this could be extended to implement all the various
;;; wavetable-based synthesis techniques).

(definstrument (two-tab start-time duration frequency amplitude
		        (partial-1 '(1.0 1.0 2.0 0.5))
			(partial-2 '(1.0 0.0 3.0 1.0))
			(amp-envelope '(0 0 50 1 100 0))
			(interp-func '(0 1 100 0))
			(vibrato-amplitude 0.005)
			(vibrato-speed 5.0)
			(degree 0.0)
			(distance 1.0)
			(reverb-amount 0.005))
  (let ((beg (seconds->samples start-time))
	(end (seconds->samples (+ start-time duration)))
	(waveform-1 (partials->wave partial-1))
	(waveform-2 (partials->wave partial-2))
	(freq (hz->radians frequency)))
    (let ((s-1 (make-table-lookup :frequency frequency :wave waveform-1))
	  (s-2 (make-table-lookup :frequency frequency :wave waveform-2))
	  (amp-env (make-env amp-envelope :scaler amplitude :duration duration))
	  (interp-env (make-env interp-func :duration duration))
	  (interp-env-1 (make-env interp-func :duration duration :offset 1.0 :scaler -1.0))
	  (loc (make-locsig degree distance reverb-amount))
	  (per-vib (make-triangle-wave :frequency vibrato-speed
				       :amplitude (* vibrato-amplitude freq)))
	  (ran-vib (make-rand-interp :frequency (+ vibrato-speed 1.0)
				     :amplitude (* vibrato-amplitude freq))))
      (do ((i beg (+ i 1)))
	  ((= i end))
	(let ((vib (+ (triangle-wave per-vib) (rand-interp ran-vib))))
	  (locsig loc i (* (env amp-env) 
			   (+ (* (env interp-env) (table-lookup s-1 vib))
			      (* (env interp-env-1) (table-lookup s-2 vib))))))))))

