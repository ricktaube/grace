#|
Date: Fri, 25 Sep 1998 09:56:41 +0300
From: Matti Koskinen <mjkoskin@sci.fi>
To: linux-audio-dev@ginette.musique.umontreal.ca
Subject: [linux-audio-dev] Announce: alpha version of denoising
[...]
	I wrote a simple denoiser called anoi after it's parent
	clm-instrument anoi.ins.

	anoi tries to remove white noise like tape hiss from wav-
	files. Removing of noise succeeds ok, but depending of the
	original sound, some distortion can be audible.

	If someone is interested, http://www.sci.fi/~mjkoskin
	contains tarred and gzipped file.

	Now only monophonic wav-files can be denoised, but adding
	others isn't too difficult. 

-matti
mjkoskin@sci.fi
|#

(definstrument (anoi infile start dur (fftsize 128) (amp-scaler 1.0) rr)
  ;; a kind of noise reduction -- on-going average spectrum is squelched to some extent
  ;; obviously aimed at intermittent signal in background noise
  ;; this is based on Perry Cook's Scrubber.m
  (let ((r (or rr (* 2.0 pi)))
	(freq-inc (floor (/ fftsize 2)))
	(fdi (make-float-vector fftsize))
	(fdr (make-float-vector fftsize)))
    (let ((spectr (make-vector freq-inc 1.0))
	  (scales (make-float-vector freq-inc 1.0))
	  (diffs (make-float-vector freq-inc))
	  (win (make-fft-window blackman2-window fftsize))
	  (incr (/ (* amp-scaler 4) *clm-srate*))
	  (beg (seconds->samples start))
	  (end (seconds->samples (+ start dur)))
	  (file (make-file->sample infile))
	  (fs (make-vector freq-inc)))
      (let ((bin (/ *clm-srate* fftsize))
	    (radius (- 1.0 (/ r fftsize))))
	(do ((ctr 0 (+ ctr 1)))
	    ((= ctr freq-inc))
	  (set! (fs ctr) (make-formant (* ctr bin) radius))))
      (set! fs (make-formant-bank fs scales))

      (set! (scales 0) 0.0)
      (do ((k 0)
	   (amp 0.0)
	   (samp 0)
	   (fdrc 0.0)
	   (i beg (+ i 1)))
	  ((= i end))
	(let ((inval (file->sample file samp)))
	  (set! samp (+ samp 1))
	  (set! (fdr k) inval)
	  (set! k (+ k 1))
	  (if (< amp amp-scaler) (set! amp (+ amp incr)))
	  (if (>= k fftsize)
	      (begin
		(set! k 0)
		(spectrum fdr fdi win 1)
		(do ((ctr 0 (+ ctr 1)))
		    ((= ctr freq-inc))
		  (set! fdrc (fdr ctr))
		  (set! (spectr ctr) (+ (* .9 (spectr ctr)) (* .1 fdrc)))
		  (set! (diffs ctr) (if (>= (spectr ctr) fdrc) 
					(/ (scales ctr) (- fftsize))
					(/ (- (/ (- fdrc (spectr ctr)) fdrc)
					      (scales ctr))
					   fftsize))))))
	  (outa i (* amp (formant-bank fs inval)))
	  (float-vector-add! scales diffs))))))


