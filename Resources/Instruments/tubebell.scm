;;; taken from Perry Cook's stkv1.tar.Z (Synthesis Toolkit), but I was
;;; in a bit of a hurry and may not have made slavishly accurate translations.
;;; Please let me know of any errors.

(definstrument (tubebell beg dur freq amp (base 32.0))
  ;; from Perry Cook's TubeBell.cpp
  (let ((osc0 (make-oscil (* freq 0.995)))
	 (osc1 (make-oscil (* freq 1.414 0.995)))
	 (osc2 (make-oscil (* freq 1.005)))
	 (osc3 (make-oscil (* freq 1.414)))
	 (ampenv1 (make-env (list 0 0 .005 1 dur 0) :base base :duration dur :scaler (* amp .5 .707)))
	 (ampenv2 (make-env (list 0 0 .001 1 dur 0) :base (* 2 base) :duration dur :scaler (* .5 amp)))
	 (ampmod (make-oscil 2.0))
	 (st (seconds->samples beg))
	 (nd (seconds->samples (+ beg dur))))
     (do ((i st (+ i 1)))
	 ((= i nd))
       (outa i (* (+ (* .007 (oscil ampmod)) .993)
		  (+ (* (env ampenv1) (oscil osc0 (* .203 (oscil osc1))))
		     (* (env ampenv2) (oscil osc2 (* .144 (oscil osc3))))))))))

