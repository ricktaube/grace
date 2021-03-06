(definstrument (jl-reverb (decay 3.0) (volume 1.0))
  (let ((allpass1 (make-all-pass -0.700 0.700 2111))
	(allpass2 (make-all-pass -0.700 0.700  673))
	(allpass3 (make-all-pass -0.700 0.700  223))
	(comb1 (make-comb 0.742 9601))
	(comb2 (make-comb 0.733 10007))
	(comb3 (make-comb 0.715 10799))
	(comb4 (make-comb 0.697 11597))
	(outdel1 (make-delay (seconds->samples .013)))
	(outdel2 (make-delay (seconds->samples .011)))
	(len (floor (+ (* decay *clm-srate*) (length *reverb*)))))
    (let ((filts (vector outdel1 outdel2))
	  (combs (make-comb-bank (vector comb1 comb2 comb3 comb4)))
	  (allpasses (make-all-pass-bank (vector allpass1 allpass2 allpass3))))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(out-bank filts i (* volume (comb-bank combs (all-pass-bank allpasses (ina i *reverb*)))))))))

