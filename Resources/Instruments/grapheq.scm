#|
From: Marco Trevisani <marco@ccrma.Stanford.EDU>

This should work like a Graphic Equalizer....
Very easy to use. Just some note:

"amp" & "amp-env" apply an enveloppe to the final result of the
filtering.  

"dur" as ""standard"" in my instruments, when dur = 0 it will take the length of the
sndfile input, otherwise the duration in seconds.

"gain-freq-list" is a list of gains and frequencies to
filter --in this order gain and frequencies--. There is no limit to
the size of the list. Gain can be a number or an
envelope. Unfortunatelly in this version they can't alternate, one
should chose, all envelopes or all numbers i.e.: 
case 1 -> '( .1 440.0 .3 1500.0 .2 330.0 ...etc) or 
case 2 -> '((0 .1 1 .5) 440.0 (0 1 1 .01) 1500 (0 .3 1 .5) 330.0 ...etc) 
'( .1 440.0 (0 1 1 .01) 1500 ..etc) <<< again, this is not allowed ..

"offset-gain" This apply to all the gains if case 1. It adds or
subtracts an offset to all the gains in the list. This number can be positive or
negative. In case the result is a negative number --let's say offset =
-.4 and, like in case 1, the first gain is .1, the result would be
-.3 -- the instrument will pass a gain equal to 0.  

"filt-gain-scale" & "filt-gain-base" will apply to the elements of the
envelopes if we are in case 2, gains are envelopes.

"stats" if #t --default-- prints the number of seconds processed, if
nil doesnt print anything, which will speed up a bit the process.
|#

(definstrument (graphEq file (beg 0) (dur 0) (or-beg 0) (amp 1) (amp-env '(0 1 .8 1 1 0)) (amp-base 1) 
	(offset-gain 0)  
	(gain-freq-list '(.8 440 .2 660))      
	(filt-gain-scale 1)                   
	(filt-gain-base 1)                    
	(a1 .99))
  (let ((st (seconds->samples beg))
	(durata (if (= 0 dur) (mus-sound-duration file) dur))
	(or-start (round (* or-beg (srate file))))
	(gain-list (let ((lst ())
			 (len (length gain-freq-list)))
		     (do ((i (- len 2) (- i 2)))
			 ((< i 0))
		       (set! lst (cons (gain-freq-list i) lst)))
		     lst))
	(freq-list (let ((lst ())
			 (len (length gain-freq-list)))
		     (do ((i (- len 1) (- i 2)))
			 ((<= i 0))
		       (set! lst (cons (gain-freq-list i) lst)))
		     lst)))
    (let ((nd (+ st (seconds->samples durata)))
	  (RdA (make-readin :file file :start or-start))
	  (half-list (/ (length gain-freq-list) 2))
	  (ampenv (make-env amp-env :scaler amp :duration durata :base amp-base))
	  (env-size (and (pair? (car gain-list))
			 (make-vector (length freq-list))))
	  (if-list-in-gain (pair? (car gain-list)))
	  (frm-size (make-vector (length freq-list)))
	  (gains (make-float-vector (length freq-list) 1.0))
	  (filt-scl (* filt-gain-scale (- 1.0 a1))))

      (do ((k 0 (+ k 1)))
	  ((= k half-list))
	(let ((gval (gain-list k))
	      (fval (freq-list k)))
	  (if (pair? gval)
	      (begin
		(set! (env-size k) (make-env gval
					     :scaler filt-scl
					     :duration durata :base filt-gain-base))
		(set! (frm-size k) (make-formant fval a1)))
	      (begin
		(set! (frm-size k) (make-formant fval a1))
		(set! (gains k) (max 0 (+ offset-gain gval)))))))
      (set! frm-size (make-formant-bank frm-size gains))

      (if if-list-in-gain
	  (do ((i st (+ i 1)))
	      ((= i nd))
	    (do ((k 0 (+ k 1)))
		((= k half-list))
	      (float-vector-set! gains k (env (vector-ref env-size k))))
	    (outa i (* (env ampenv) (formant-bank frm-size (readin RdA)))))
	  (do ((i st (+ i 1)))
	      ((= i nd))
	    (outa i (* (env ampenv) (formant-bank frm-size (readin RdA)))))))))

