(definstrument (za time dur freq amp length1 length2 feedback feedforward)
  (let ((beg (seconds->samples time))
	 (end (seconds->samples (+ time dur)))
	 (s (make-pulse-train freq amp))
	 (d0 (make-all-pass feedback feedforward :size length1 :max-size (+ 1 (max length1 length2))))
	 (zenv (make-env '(0 0 1 1) :scaler (- length2 length1) :duration dur)))
     (do ((i beg (+ i 1)))
	 ((= i end))
       (outa i (all-pass d0 (pulse-train s) (env zenv))))))

;;(with-sound () (za 0 1 100 .1 20 100 .95 .95) (za 1.5 1 100 .1 100 20 .95 .95))

