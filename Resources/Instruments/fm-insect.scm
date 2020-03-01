;;; -------- FM_INSECT
(definstrument (fm-insect startime dur frequency amplitude amp-env 
			  mod-freq mod-skew mod-freq-env mod-index mod-index-env 
			  fm-index fm-ratio
			  (degree 0.0)
		     	       (distance 1.0)
		               (reverb-amount 0.005))
  (let ((beg (seconds->samples startime))
	 (end (seconds->samples (+ startime dur)))
	 (loc (make-locsig degree distance reverb-amount))
	 (carrier (make-oscil frequency))
	 (fm1-osc (make-oscil mod-freq))
	 (fm2-osc (make-oscil (* fm-ratio frequency)))
	 (ampf (make-env amp-env :scaler amplitude :duration dur))
	 (indf (make-env mod-index-env :scaler (hz->radians mod-index) :duration dur))
	 (modfrqf (make-env mod-freq-env :scaler (hz->radians mod-skew) :duration dur))
	 (fm2-amp (hz->radians (* fm-index fm-ratio frequency))))
     (do ((i beg (+ i 1)))
	 ((= i end))
       (let ((garble-in (* (env indf)
			    (oscil fm1-osc (env modfrqf)))))
	 (locsig loc i (* (env ampf) 
			  (oscil carrier (+ (* fm2-amp (oscil fm2-osc garble-in))
					    garble-in))))))))

#|
(with-sound (:srate 22050) 
  (let ((locust '(0 0 40 1 95 1 100 .5))
	(bug_hi '(0 1 25 .7 75 .78 100 1))
	(amp    '(0 0 25 1 75 .7 100 0)))
    (fm-insect 0      1.699  4142.627  .015 amp 60 -16.707 locust 500.866 bug_hi  .346  .500)
    (fm-insect 0.195   .233  4126.284  .030 amp 60 -12.142 locust 649.490 bug_hi  .407  .500)
    (fm-insect 0.217  2.057  3930.258  .045 amp 60 -3.011  locust 562.087 bug_hi  .591  .500)
    (fm-insect 2.100  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .346  .500)
    (fm-insect 3.000  1.500   900.627  .06  amp 40 -16.707 locust 300.866 bug_hi  .046  .500)
    (fm-insect 3.450  1.500   900.627  .09  amp 40 -16.707 locust 300.866 bug_hi  .006  .500)
    (fm-insect 3.950  1.500   900.627  .12  amp 40 -10.707 locust 300.866 bug_hi  .346  .500)
    (fm-insect 4.300  1.500   900.627  .09  amp 40 -20.707 locust 300.866 bug_hi  .246  .500)))
|#
