
(defgenerator grn 
  (rampval 0.0) 
  (rampinc 0.0)
  (loc 0) 
  (segctr 0)
  (whichseg 0)
  (ramplen 0)
  (steadylen 0)
  (trigger 0)
  file)

(definstrument (expfil start duration hopsecs rampsecs steadysecs file1 file2)
  (let ((fil1 (make-file->sample file1))
	 (fil2 (make-file->sample file2))
	 (hop (seconds->samples hopsecs))
	 (rampdur (seconds->samples rampsecs))
	 (steadydur (seconds->samples steadysecs))
	 (beg (seconds->samples start))
	 (end (seconds->samples (+ start duration))))
    (let ((grn1 (make-grn :rampval 0.0 :rampinc (/ 1.0 rampdur) :loc 0 :segctr 0 :whichseg 0 :ramplen rampdur :steadylen steadydur :trigger 0 :file fil1))
	  (grn2 (make-grn :rampval 0.0 :rampinc (/ 1.0 rampdur) :loc 0 :segctr 0 :whichseg 0 :ramplen rampdur :steadylen steadydur :trigger 0 :file fil2))
	  (out1 beg)
	  (out2 (+ hop beg)))
      (do ((i beg (+ i 1))
	   (val 0.0 0.0))
	  ((= i end))
	(when (= i out1)
	  (set! val (with-let grn1
		      (let ((inval (ina loc file)))
			(set! loc (+ loc 1))
			(case whichseg 
			  ((0)	;ramp-up
			   (set! inval (* inval rampval))
			   (set! rampval (+ rampval rampinc))
			   (set! segctr (+ segctr 1))
			   (if (= segctr ramplen)
			       (begin
				 (set! segctr 0)
				 (set! whichseg (+ whichseg 1)))))
			  ((1)		;steady-state
			   (set! segctr (+ segctr 1))
			   (if (= segctr steadylen)
			       (begin
				 (set! segctr 0)
				 (set! whichseg (+ whichseg 1)))))
			  (else				;ramp-down
			   (set! inval (* inval rampval))
			   (set! segctr (+ segctr 1))
			   (set! rampval (- rampval rampinc))
			   (if (= segctr ramplen)
			       (begin
				 (set! segctr 0)
				 (set! trigger 1)
				 (set! whichseg 0)
				 (set! rampval 0.0)))))
			inval)))
	  (set! out1 (+ out1 1))
	  (if (= (grn1 'trigger) 1)
	      (begin
		(set! (grn1 'trigger) 0)
		(set! out1 (+ out1 hop)))))
	(when (= i out2)
	  (set! val (+ val (with-let grn2
			     (let ((inval (ina loc file)))
			       (set! loc (+ loc 1))
			       (case whichseg
				 ((0)	;ramp-up
				  (set! inval (* inval rampval))
				  (set! rampval (+ rampval rampinc))
				  (set! segctr (+ segctr 1))
				  (if (= segctr ramplen)
				      (begin
					(set! segctr 0)
					(set! whichseg (+ whichseg 1)))))
				 ((1)		;steady-state
				  (set! segctr (+ segctr 1))
				  (if (= segctr steadylen)
				      (begin
					(set! segctr 0)
					(set! whichseg (+ whichseg 1)))))
				 (else				;ramp-down
				  (set! inval (* inval rampval))
				  (set! segctr (+ segctr 1))
				  (set! rampval (- rampval rampinc))
				  (if (= segctr ramplen)
				      (begin
					(set! segctr 0)
					(set! trigger 1)
					(set! whichseg 0)
					(set! rampval 0.0)))))
			       inval))))
	  (set! out2 (+ out2 1))
	  (if (= (grn2 'trigger) 1)
	      (begin
		(set! (grn2 'trigger) 0)
		(set! out2 (+ out2 hop)))))
	(outa i val)))))
  
;;; (with-sound () (expfil 0 2 .2 .01 .1 "oboe.snd" "fyow.snd"))
  
