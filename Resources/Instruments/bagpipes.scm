(definstrument (drone startime dur frequency amp ampfun synth ampat ampdc amtrev deg dis rvibamt rvibfreq)
  (let ((beg (seconds->samples startime))
	(end (seconds->samples (+ startime dur)))
	(waveform (partials->wave synth))
	(amplitude (* amp .25))
	(freq (hz->radians frequency)))
    (let ((s (make-table-lookup :frequency frequency :wave waveform))
	  (amp-env (let ((ampe (stretch-envelope ampfun 25 (* 100 (/ ampat dur)) 75 (- 100 (* 100 (/ ampdc dur))))))
		     (make-env ampe :scaler amplitude :duration dur)))
	  (ran-vib (make-rand :frequency rvibfreq :amplitude (* rvibamt freq)))
	  (loc (make-locsig deg dis amtrev)))
      (do ((i beg (+ i 1)))
	  ((= i end))
	(locsig loc i (* (env amp-env) (table-lookup s (rand ran-vib))))))))


(definstrument (canter beg dur pitch amp-1 deg dis pcrev ampfun ranfun skewfun
		       skewpc ranpc ranfreq indexfun atdr dcdr
		       ampfun1 indfun1 fmtfun1
		       ampfun2 indfun2 fmtfun2
		       ampfun3 indfun3 fmtfun3
		       ampfun4 indfun4 fmtfun4)
  (let ((amp (* amp-1 .25))		;pvc's amplitudes in bag.clm are very high (overflows)
	(rangetop 910.0)
	(rangebot 400.0))
    (let ((k (floor (* 100 (log (/ pitch rangebot) (/ rangetop rangebot)))))
	  (atpt (* 100 (/ atdr dur)))
	  (dcpt (- 100 (* 100 (/ dcdr dur)))))
      (let ((lfmt1 (envelope-interp k fmtfun1))
	    (lfmt2 (envelope-interp k fmtfun2))
	    (lfmt3 (envelope-interp k fmtfun3))
	    (lfmt4 (envelope-interp k fmtfun4))
	    (dev11 (hz->radians (* (envelope-interp k indfun1) pitch)))
	    (dev12 (hz->radians (* (envelope-interp k indfun2) pitch)))
	    (dev13 (hz->radians (* (envelope-interp k indfun3) pitch)))
	    (dev14 (hz->radians (* (envelope-interp k indfun4) pitch))))
	(let ((start (seconds->samples beg))
	      (end (seconds->samples (+ beg dur)))
	      (dev01 (* dev11 .5))
	      (dev02 (* dev12 .5))
	      (dev03 (* dev13 .5))
	      (dev04 (* dev14 .5))
	      (harm1 (floor (+ .5 (/ lfmt1 pitch))))
	      (harm2 (floor (+ .5 (/ lfmt2 pitch))))
	      (harm3 (floor (+ .5 (/ lfmt3 pitch))))
	      (harm4 (floor (+ .5 (/ lfmt4 pitch)))))
	  (let ((lamp1 (* (envelope-interp k ampfun1) amp (- 1 (abs (- harm1 (/ lfmt1 pitch))))))
		(lamp2 (* (envelope-interp k ampfun2) amp (- 1 (abs (- harm2 (/ lfmt2 pitch))))))
		(lamp3 (* (envelope-interp k ampfun3) amp (- 1 (abs (- harm3 (/ lfmt3 pitch))))))
		(lamp4 (* (envelope-interp k ampfun4) amp (- 1 (abs (- harm4 (/ lfmt4 pitch))))))
		(tidx-stretched (stretch-envelope indexfun 25 atpt 75 dcpt)))
	    (let ((tampfun (make-env (stretch-envelope ampfun 25 atpt 75 dcpt) :duration dur))
		  (tskwfun (make-env (stretch-envelope skewfun 25 atpt 75 dcpt) :scaler (hz->radians (* pitch skewpc)) :duration dur))
		  (tranfun (make-env (stretch-envelope ranfun 25 atpt 75 dcpt) :duration dur))
		  (d1env (make-env tidx-stretched :offset dev01 :scaler dev11 :duration dur))
		  (d2env (make-env tidx-stretched :offset dev02 :scaler dev12 :duration dur))
		  (d3env (make-env tidx-stretched :offset dev03 :scaler dev13 :duration dur))
		  (d4env (make-env tidx-stretched :offset dev04 :scaler dev14 :duration dur))
		  (modgen (make-oscil pitch))
		  (ranvib (make-rand :frequency ranfreq :amplitude (hz->radians (* ranpc pitch))))
		  (loc (make-locsig deg dis pcrev))
		  (gen1 (make-oscil (* pitch harm1)))
		  (gen2 (make-oscil (* pitch harm2)))
		  (gen3 (make-oscil (* pitch harm3)))
		  (gen4 (make-oscil (* pitch harm4))))
	      (do ((i start (+ i 1)))
		  ((= i end))
		(let* ((frqval (+ (env tskwfun) (* (env tranfun) (rand ranvib))))
		       (modval (oscil modgen frqval)))
		  (locsig loc i (* (env tampfun)
				   (+ (* lamp1 (oscil gen1 (* (+ (* (env d1env) modval) frqval) harm1)))
				      (* lamp2 (oscil gen2 (* (+ (* (env d2env) modval) frqval) harm2)))
				      (* lamp3 (oscil gen3 (* (+ (* (env d3env) modval) frqval) harm3)))
				      (* lamp4 (oscil gen4 (* (+ (* (env d4env) modval) frqval) harm4)))))))))))))))
#|
(with-sound (:reverb nrev)
  ;; envelopes
  (define fmt1 '(0 1200 100 1000))
  (define fmt2 '(0 2250 100 1800))
  (define fmt3 '(0 4500 100 4500))
  (define fmt4 '(0 6750 100 8100))
  (define amp1 '(0 .67 100 .7))
  (define amp2 '(0 .95 100 .95))
  (define amp3 '(0 .28 100 .33))
  (define amp4 '(0 .14 100 .15))
  (define ind1 '(0 .75 100 .65))
  (define ind2 '(0 .75 100 .75))
  (define ind3 '(0 1 100 1))
  (define ind4 '(0 1 100 1))
  (define skwf '(0 0 100 0))
  (define ampf '(0 0 25 1 75 1 100 0))
  (define ranf '(0 .5 100 .5))
  (define index '(0 1 100 1))
  (define solid '(0 0 5 1 95 1 100 0))
  (define bassdr2 '(.5 .06 1 .62 1.5 .07 2.0 .6 2.5 .08 3.0 .56 4.0 .24 
                    5 .98 6 .53 7 .16 8 .33 9 .62 10 .12 12 .14 14 .86
                    16 .12 23 .14 24 .17))
  (define tenordr '(.3 .04 1 .81 2 .27 3 .2 4 .21 5 .18 6 .35 7 .03
                     8 .07 9 .02 10 .025 11 .035))

  (drone  .000  41.000  115.000  .500 solid bassdr2  .100  .500
         	.030  4.000 1  .010 10)
  (drone  .000  41.000  229.000  .500 solid tenordr  .100  .500
         	.030  4.000 1  .010 11)
  (drone  .000  41.000  229.500  .500 solid tenordr  .100  .500
         	.030  4.000 1  .010 9)
  (canter  .000  2.100 918  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  2.100  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  2.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  2.440  .560 459  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.000  .040 408  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.040  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.080  .040 408  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.120  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.160  .290 459  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.450  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.640  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.680  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.720  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.900  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  (canter  3.940  .260 459  .700  45.000 1  .050 ampf ranf skwf
         	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
          	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
  
  




(canter  4.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.240  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.280  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.320  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.500  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.800  .040 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.840  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  4.880  .520 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  5.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  5.440  .560 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.000  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.040  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.080  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.120  .180 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.300  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.640  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.680  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.720  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.900  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  6.940  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.200  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.240  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.280  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.320  .480 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.800  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  7.840  .410 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  8.250  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  8.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  8.440  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  8.480  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  8.520  .480 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.040  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.080  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.120  .330 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.450  .150 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.640  .040 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.680  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  9.720  .480 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.240  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.280  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.320  .480 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.800  .040 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  10.840  .410 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.250  .150 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.440  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.480  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.520  .030 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.550  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.700  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.740  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.780  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.820  .030 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  11.850  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )

(canter  12.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.040  .560 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.600  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.640  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.680  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.720  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  12.760  .290 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.050  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.240  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.280  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.320  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.500  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.540  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.800  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.840  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.880  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  13.920  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  14.100  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  14.400  .040 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  14.440  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  14.480  .520 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.040  .560 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.600  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.640  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.680  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.720  .180 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  15.900  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.240  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.280  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.320  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.500  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.540  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.800  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.840  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.880  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  16.920  .480 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  17.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  17.440  .410 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  17.850  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.040  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.080  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.120  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.300  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.640  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.680  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.720  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.900  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  18.940  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.240  .040 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.280  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.320  .480 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.800  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  19.840  .410 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  20.250  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  20.400  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  20.440  .560 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.040  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.080  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.120  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.300  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.600  .040 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.640  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  21.680  .520 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.240  .560 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.800  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.840  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.880  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  22.920  .180 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.100  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.440  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.480  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.520  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.700  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  23.740  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  24.000  .040 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  24.040  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  24.080  .520 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  24.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  24.640  .560 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.200  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.240  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.280  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.320  .180 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.500  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.800  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.840  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.880  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  25.920  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  26.100  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  26.400  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  26.440  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  26.480  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  26.520  .480 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.040  .410 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.450  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.640  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.680  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  27.720  .480 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.240  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.280  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.320  .180 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.500  .300 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.800  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.840  .040 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.880  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  28.920  .480 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  29.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  29.440  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  29.480  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  29.520  .480 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.000  .040 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.040  .410 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.450  .150 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.640  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.680  .040 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.720  .030 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.750  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.900  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.940  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  30.980  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.020  .030 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.050  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.240  .560 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.800  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.840  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.880  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.920  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  31.960  .290 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.250  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.440  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.480  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.520  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.700  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  32.740  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.000  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.040  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.080  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.120  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.300  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.600  .040 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.640  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  33.680  .520 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.240  .560 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.800  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.840  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.880  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  34.920  .180 918  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.100  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.440  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.480  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.520  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.700  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  35.740  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.000  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.040  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.080  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.120  .480 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.600  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  36.640  .410 765  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.050  .150 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.200  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.240  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.280  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.320  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.500  .300 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.800  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.840  .040 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.880  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  37.920  .180 573.75  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.100  .040 688.5  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.140  .260 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.400  .040 826.2  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.440  .040 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.480  .040 619.65  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  38.520  .480 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  39.000  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  39.040  .410 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  39.450  .150 516.375  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  39.600  .040 408  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
(canter  39.640  .860 459  .700  45.000 1  .050 ampf ranf skwf
	 .050  .010 10 index  .005  .005 amp1 ind1 fmt1 amp2
	ind2 fmt2 amp3 ind3 fmt3 amp4 ind4 fmt4  )
)

|#








