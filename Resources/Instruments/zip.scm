;;; create the 'digital zipper' effect
;;; a not-very-debonair way to fade out file1 and fade in file2
;;; this is also good if the same file is used twice -- sort of like a CD player gone berserk
;;;
;;; changed 19-Apr-05 to use def-clm-struct and envelopes (and fixed duration bug in zip-sound)


(provide 'snd-zip.scm)
(if (not (provided? 'snd-ws.scm)) (load-from-path "ws.scm"))

(define (safe-srate) (if (not (null? (sounds))) (srate) (mus-srate)))


(def-clm-struct zdata
  (low-start 20 :type int)
  (frame-loc 0 :type int)
  (cursamples 0 :type int)
  (frame0 #f :type vct)
  (frame1 #f :type vct)
  (frame2 #f :type vct)
  (fe #f :type clm)
  (rampe #f :type clm))

(define* (make-zipper ramp-env :optional frame-size frame-env)
  "(make-zipper ramp-env :optional frame-size frame-env) makes a zipper generator.  'ramp-env' is 
an envelope (normally a ramp from 0 to 1) which sets where we are in the zipping process, 
'frame-size' is the maximum frame length during the zip in seconds (defaults to 0.05), and 
'frame-env' is an envelope returning the current frame size during the zip process."

  (let ((max-size (+ 1 (inexact->exact (ceiling (* (safe-srate) (or frame-size 0.05)))))))
    (make-zdata :low-start 20
		:frame-loc 0
		:cursamples 0
		:frame0 (make-vct max-size)
		:frame1 (make-vct max-size)
		:frame2 (make-vct max-size)
		:fe (or frame-env (make-env (list 0 (* (safe-srate) 0.05)) :length (mus-length ramp-env))) ; a bit of a kludge...
		:rampe ramp-env)))


(define (zipper zp input1 input2)
  "(zipper zip in1 in2) creates the digital zipper sound effect using zipper generator 'zip' and the two sample readers 'in1' and 'in2'"
  (let* ((ramp-loc (env (zdata-rampe zp)))
	 (frame-samples (inexact->exact (floor (env (zdata-fe zp)))))
	 (frame1 (zdata-frame1 zp))
	 (frame2 (zdata-frame2 zp))
	 (chunk-len (inexact->exact (round (* frame-samples ramp-loc)))))
    (if (<= chunk-len (zdata-low-start zp))
	(begin
	  (set! (zdata-frame-loc zp) 0)
	  (read-sample input1))
	(if (>= chunk-len (- frame-samples (zdata-low-start zp)))
	    (begin
	      (set! (zdata-frame-loc zp) 0)
	      (input2))
	    ;; else we're in the ramp phase
	    ;;  read frame if we're within its bounds
	    (begin
	      (if (>= (zdata-frame-loc zp) (zdata-cursamples zp))
		  ;; now get next portion of the ramp
		  (begin
		    (set! (zdata-frame-loc zp) 0)
		    (set! (zdata-cursamples zp) frame-samples)
		    (do ((k 0 (+ 1 k)))
			((= k frame-samples))
		      (vct-set! frame1 k (read-sample input1))
		      (vct-set! frame2 k (read-sample input2)))
		    ;; now resample each dependent on location in ramp (samp1 and samp2 are increments)
		    (vct-fill! (zdata-frame0 zp) 0.0)
		    (let ((start-ctr 0.0)
			  (samp2 (inexact->exact (floor (/ frame-samples chunk-len)))))
		      ;; (snd-display ";samp2: ~A, len: ~A ~A -> ~A ~A" samp2 chunk-len frame-samples (+ 1 (* chunk-len samp2)) (vct-length frame2))
		      (do ((k 0 (+ 1 k)))
			  ((= k chunk-len))
			(let* ((ictr (inexact->exact (floor start-ctr)))
			       (y0 (vct-ref frame2 ictr))
			       (y1 (vct-ref frame2 (+ ictr 1))))
			  (vct-set! (zdata-frame0 zp) k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			  (set! start-ctr (+ start-ctr samp2)))))
		    (let ((start-ctr 0.0)
			  (samp1 (inexact->exact (floor (/ frame-samples (- frame-samples chunk-len))))))
		      (do ((k chunk-len (+ 1 k)))
			  ((= k frame-samples))
			(let* ((ictr (inexact->exact (floor start-ctr)))
			       (y0 (vct-ref frame1 ictr))
			       (y1 (vct-ref frame1 (+ ictr 1))))
			  (vct-set! (zdata-frame0 zp) k (+ y0 (* (- y1 y0) (- start-ctr ictr))))
			  (set! start-ctr (+ start-ctr samp1)))))))
	      (let ((result (vct-ref (zdata-frame0 zp) (zdata-frame-loc zp))))
		(set! (zdata-frame-loc zp) (+ (zdata-frame-loc zp) 1))
		result))))))


;; (zip-sound 0 1 "fyow.snd" "now.snd" '(0 0 1 1) .05)
;; (zip-sound 0 3 "mb.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)

(define* (zip-sound beg-in-seconds dur-in-seconds file1 file2 :optional ramp size)
  "(zip-sound beg dur file1 file2 :optional ramp-env size) zips the two files and mixes the result into the current sound"
  (let* ((beg (inexact->exact (round (* (srate) beg-in-seconds))))
	 (dur (inexact->exact (round (* (srate) dur-in-seconds))))
	 (zip (make-zipper (make-env (or ramp (list 0 0 1 1)) :length dur)
			   (or size 0.05)
			   (make-env (list 0 (* (srate) (or size 0.05))) :length dur)))
	(read0 (make-sample-reader 0 file1))
	(read1 (make-sample-reader 0 file2)))
    (map-channel (lambda (y)
		   (+ y (zipper zip read0 read1)))
		 beg dur)))

#|
(define (ramp-test)
  (let ((data (make-vct 10000)))
    (new-sound "new-0.snd")
    (do ((i 0 (+ 1 i))) ((= i 10000)) (vct-set! data i (* i .0001)))
    (vct->channel data 0 10000 0)
    (new-sound "new-1.snd")
    (do ((i 0 (+ 1 i))) ((= i 10000)) (vct-set! data i (- 1.0 (* i .0001))))
    (vct->channel data 0 10000 1)
    (let* ((dur (frames))
	   (zp (make-zipper (make-env '(0 0 1 1) :length dur)
			    0.05
			    (make-env (list 0 (* (safe-srate) 0.05)) :length dur)))
	  (reader0 (make-sample-reader 0 0 0))
	  (reader1 (make-sample-reader 0 1 0)))
      (map-channel (lambda (val)
		     (zipper zp reader0 reader1))))))
|#

