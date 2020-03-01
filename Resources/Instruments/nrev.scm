;;; NREV (the most popular Samson box reverb)

(definstrument (nrev (reverb-factor 1.09) (lp-coeff 0.7) (volume 1.0))
  ;; reverb-factor controls the length of the decay -- it should not exceed (/ 1.0 .823)
  ;; lp-coeff controls the strength of the low pass filter inserted in the feedback loop
  ;; output-scale can be used to boost the reverb output

  (define (next-prime val)
    (do ((val val (+ val 2)))
	((or (= val 2)
	     (and (odd? val)
		  (do ((i 3 (+ i 2))
		       (lim (sqrt val)))
		      ((or (= 0 (modulo val i))
			   (> i lim))
		       (> i lim)))))
           val)))
  
  (let ((srscale (/ *clm-srate* 25641))
	(dly-len (list 1433 1601 1867 2053 2251 2399 347 113 37 59 53 43 37 29 19))
	(chan2 (> (channels *output*) 1))
	(chan4 (= (channels *output*) 4)))
	
    (do ((i 0 (+ i 1)))
	((= i 15))
      (let ((val (floor (* srscale (dly-len i)))))
	(if (even? val) (set! val (+ val 1)))
	(set! (dly-len i) (next-prime val))))

    (let ((len (+ (floor *clm-srate*) (framples *reverb*)))
	   (comb1 (make-comb (* .822 reverb-factor) (dly-len 0)))
	   (comb2 (make-comb (* .802 reverb-factor) (dly-len 1)))
	   (comb3 (make-comb (* .773 reverb-factor) (dly-len 2)))
	   (comb4 (make-comb (* .753 reverb-factor) (dly-len 3)))
	   (comb5 (make-comb (* .753 reverb-factor) (dly-len 4)))
	   (comb6 (make-comb (* .733 reverb-factor) (dly-len 5)))
	   (low (make-one-pole lp-coeff (- lp-coeff 1.0)))
	   (allpass1 (make-all-pass -0.700 0.700 (dly-len 6)))
	   (allpass2 (make-all-pass -0.700 0.700 (dly-len 7)))
	   (allpass3 (make-all-pass -0.700 0.700 (dly-len 8)))
	   (allpass4 (make-all-pass -0.700 0.700 (dly-len 9))) ; 10 for quad
	   (allpass5 (make-all-pass -0.700 0.700 (dly-len 11)))
	   (allpass6 (and chan2 (make-all-pass -0.700 0.700 (dly-len 12))))
	   (allpass7 (and chan4 (make-all-pass -0.700 0.700 (dly-len 13))))
	   (allpass8 (and chan4 (make-all-pass -0.700 0.700 (dly-len 14)))))

      (let ((filts (if (not chan2)
		       (vector allpass5)
		       (if (not chan4)
			   (vector allpass5 allpass6)
			   (vector allpass5 allpass6 allpass7 allpass8))))
	    (combs (make-comb-bank (vector comb1 comb2 comb3 comb4 comb5 comb6)))
	    (allpasses (make-all-pass-bank (vector allpass1 allpass2 allpass3))))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (out-bank filts i
		    (all-pass allpass4
			      (one-pole low
					(all-pass-bank allpasses
						       (comb-bank combs (* volume (ina i *reverb*))))))))))))
