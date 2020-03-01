;;; freeverb.scm -- CLM -> Snd/Scheme translation of freeverb.ins

;; Translator/Author: Michael Scholz <scholz-micha@gmx.de>
;; Last: Thu Apr 24 01:32:15 CEST 2003
;; Version: $Revision: 1.2 $

;;; Original notes of Fernando Lopez-Lezcano

;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
;;
;; Written by Jezar at Dreampoint, June 2000
;; http://www.dreampoint.co.uk
;;
;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
;; Version 1.0 for clm-2 released in January 2001
;; http://ccrma.stanford.edu/~nando/clm/freeverb/
;;
;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
;; - the clm version can now work with a mono input or an n-channel input
;;   stream (in the latter case the number of channels of the input and output
;;   streams must match.
;; - the "wet" parameter has been eliminated as it does not apply to the model
;;   that clm uses to generate reverberation
;; - the "width" parameter name has been changed to :global. It now controls the
;;   coefficients of an NxN matrix that specifies how the output of the reverbs
;;   is mixed into the output stream.
;; - predelays for the input channels have been added.
;; - damping can be controlled individually for each channel.

;; For more information see clm-2/freeverb/index.html [MS]

;;; changed to accommodate run and mono output, bill 11-Jun-06
;;;            use the filtered-comb gen, bill 29-Jun-06
;;; optimized slightly, bill 17-Sep-12
;;; changed to use float-vectors, not frames and mixers 11-Oct-13

;;; Code:

(provide 'snd-freeverb.scm)
(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))

(definstrument (freeverb
		(room-decay 0.5)
		(damping 0.5)
		(global 0.3)
		(predelay 0.03)
		(output-gain 1.0)
		output-mixer
		(scale-room-decay 0.28)
		(offset-room-decay 0.7)
		(combtuning '(1116 1188 1277 1356 1422 1491 1557 1617))
		(allpasstuning '(556 441 341 225))
		(scale-damping 0.4)
		(stereo-spread 23)
		(decay-time 1.0)
		verbose)
  (let ((dur (+ decay-time (mus-sound-duration (mus-file-name *reverb*))))
	(out-chans (channels *output*))
	(in-chans (channels *reverb*))
	(srate-scale (/ *clm-srate* 44100.0))
	(room-decay-val (+ (* room-decay scale-room-decay) offset-room-decay))
	(numcombs (length combtuning))
	(numallpasses (length allpasstuning)))
    (let ((end (seconds->samples dur))
	  (out-buf (make-float-vector out-chans))
	  (f-out (make-float-vector out-chans))
	  (f-in (make-float-vector in-chans))
	  (predelays (make-vector in-chans))
	  (fcombs (make-vector (* out-chans numcombs)))
	  (allpasses (make-vector (* out-chans numallpasses)))
	  (local-gain (if (= out-chans 1)
			  global
			  (+ (/ (- 1.0 global) (- 1 (/ 1.0 out-chans)))
			     (/ 1.0 out-chans))))
	  (global-gain 0.0))

      (set! global-gain (if (= out-chans 1)
			    local-gain
			    (/ (- out-chans (* local-gain out-chans))
			       (- (* out-chans out-chans) out-chans))))
      (if verbose
	  (format () ";;; freeverb: ~d input channels, ~d output channels~%" in-chans out-chans))
      (if (and (> in-chans 1)
	       (not (= in-chans out-chans)))
	  (error 'wrong-type-arg "input must be mono or input channels must equal output channels"))

      (let ((out-mix (or output-mixer
		       (let ((v (make-float-vector (list out-chans out-chans))))
			 (do ((i 0 (+ i 1)))
			     ((= i out-chans))
			   (do ((j 0 (+ j 1)))
			       ((= j out-chans))
			     (set! (v i j) (/ (* output-gain (if (= i j) local-gain global-gain)) out-chans))))
			 v))))

	(do ((c 0 (+ 1 c)))
	    ((= c in-chans))
	  (set! (predelays c) (make-delay :size (round (* *clm-srate* (if (number? predelay) predelay (predelay c)))))))

	(do ((c 0 (+ 1 c)))
	    ((= c out-chans))
	  (do ((i 0 (+ i 1)))
	      ((= i numcombs))
	    (let ((len (floor (* srate-scale (combtuning i))))
		  (dmp (* scale-damping (if (number? damping) damping (damping i)))))
	      (if (odd? c)
		  (set! len (+ len (floor (* srate-scale stereo-spread)))))
	      (set! (fcombs (+ (* c numcombs) i))
		    (make-filtered-comb :size len 
					:scaler room-decay-val 
					:filter (make-one-zero :a0 (- 1.0 dmp) :a1 dmp))))))
	(do ((c 0 (+ 1 c)))
	    ((= c out-chans))
	  (do ((i 0 (+ i 1)))
	      ((= i numallpasses))
	    (let ((len (floor (* srate-scale (allpasstuning i)))))
	      (if (odd? c)
		  (set! len (+ len (floor (* srate-scale stereo-spread)))))
	      (set! (allpasses (+ (* c numallpasses) i))
		    (make-all-pass :size len :feedforward -1 :feedback 0.5)))))
	
	(if (= out-chans in-chans 1)
	    
	    (let ((amp (out-mix 0 0))
		  (pdelay (predelays 0)))
	      (set! allpasses (make-all-pass-bank allpasses))
	      (set! fcombs (make-filtered-comb-bank fcombs))
	      
	      (do ((i 0 (+ i 1)))
		  ((= i end))
		(outa i (* amp (all-pass-bank allpasses
					      (filtered-comb-bank fcombs
								  (delay pdelay (ina i *reverb*))))))))

	    (let ((allp-c (make-vector out-chans))
		  (fcmb-c (make-vector out-chans)))
	      (do ((c 0 (+ c 1)))
		  ((= c out-chans))
		(set! (allp-c c) (make-vector numallpasses))
		(set! (fcmb-c c) (make-vector numcombs)))
	      (do ((c 0 (+ c 1)))
		  ((= c out-chans))
		(do ((j 0 (+ j 1)))
		    ((= j numcombs))
		  (set! ((fcmb-c c) j) (fcombs (+ j (* c numcombs)))))
		(do ((j 0 (+ j 1)))
		    ((= j numallpasses))
		  (set! ((allp-c c) j) (allpasses (+ j (* c numallpasses)))))
		(set! (allp-c c) (make-all-pass-bank (allp-c c)))
		(set! (fcmb-c c) (make-filtered-comb-bank (fcmb-c c))))
	      

	      (if (= in-chans out-chans 5)
		  (let ((allp0 (vector-ref allp-c 0))
			(allp1 (vector-ref allp-c 1))
			(allp2 (vector-ref allp-c 2))
			(allp3 (vector-ref allp-c 3))
			(allp4 (vector-ref allp-c 4))
			(fcmb0 (vector-ref fcmb-c 0))
			(fcmb1 (vector-ref fcmb-c 1))
			(fcmb2 (vector-ref fcmb-c 2))
			(fcmb3 (vector-ref fcmb-c 3))
			(fcmb4 (vector-ref fcmb-c 4))
			(dly0 (vector-ref predelays 0))
			(dly1 (vector-ref predelays 1))
			(dly2 (vector-ref predelays 2))
			(dly3 (vector-ref predelays 3))
			(dly4 (vector-ref predelays 4)))
		    (do ((i 0 (+ i 1)))
			((= i end))
		      (file->frample *reverb* i f-in)
		      (float-vector-set! f-out 0 (all-pass-bank allp0 (filtered-comb-bank fcmb0 (delay dly0 (float-vector-ref f-in 0)))))
		      (float-vector-set! f-out 1 (all-pass-bank allp1 (filtered-comb-bank fcmb1 (delay dly1 (float-vector-ref f-in 1)))))
		      (float-vector-set! f-out 2 (all-pass-bank allp2 (filtered-comb-bank fcmb2 (delay dly2 (float-vector-ref f-in 2)))))
		      (float-vector-set! f-out 3 (all-pass-bank allp3 (filtered-comb-bank fcmb3 (delay dly3 (float-vector-ref f-in 3)))))
		      (float-vector-set! f-out 4 (all-pass-bank allp4 (filtered-comb-bank fcmb4 (delay dly4 (float-vector-ref f-in 4)))))
		      (frample->file *output* i (frample->frample out-mix f-out out-chans out-buf out-chans))))
		  
		  (if (> in-chans 1)
		      (do ((i 0 (+ i 1)))
			  ((= i end))
			(file->frample *reverb* i f-in)
			(do ((c 0 (+ c 1)))
			    ((= c out-chans))
			  (float-vector-set! f-out c (all-pass-bank (vector-ref allp-c c) 
								    (filtered-comb-bank (vector-ref fcmb-c c) 
											(delay (vector-ref predelays c) 
											       (float-vector-ref f-in c))))))
			(frample->file *output* i (frample->frample out-mix f-out out-chans out-buf out-chans)))
		      
		      (let ((pdelay (predelays 0)))
			(do ((i 0 (+ i 1)))
			    ((= i end))
			  (let ((val (delay pdelay (ina i *reverb*))))
			    (do ((c 0 (+ c 1)))
				((= c out-chans))
			      (float-vector-set! f-out c (all-pass-bank (vector-ref allp-c c) 
									(filtered-comb-bank (vector-ref fcmb-c c) 
											    val))))
			    (frample->file *output* i (frample->frample out-mix f-out out-chans out-buf out-chans)))))))))))))
  
;;; (with-sound (:statistics #t :reverb freeverb :reverb-data '(:output-gain 3.0)) (outa 0 .5 *reverb*))
;;; (with-sound (:channels 2 :reverb-channels 2 :statistics #t :reverb freeverb :reverb-data '(:output-gain 3.0)) (outa 0 .5 *reverb*) (outb 0 .1 *reverb*))

