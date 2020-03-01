;;; **********************************************************************
;;; Copyright 1999-2014 Rick Taube.  All rights reserved.
;;; Licensed under the "Attribution-NonCommercial-ShareAlike" Vizsage
;;; Public License, which says that non-commercial users may share and
;;; modify this code but must give credit and share improvements. For
;;; complete terms please read the text of the full license available at
;;; this link: http://vizsage.com/license/Vizsage-License-BY-NC-SA.html
;;; **********************************************************************

;
;; Plotting Utilities
;

(define (axis-from-spread vmin vmax units)
  ;; return axis definitino given spread in data and optional axis
  ;; units. units is the increment along the axis to use. if false set
  ;; to the largest power of 10 that is stricly less than the range
  ;; between vmin and vmax
  (if (not units)
    (let* ((e (log (- vmax vmin) 10))
           (f (floor e)))
      (if (= e f) (set! f (- f 1.0))) ; insure < range
      (set! units (expt 10 f))))
  (list (* (floor (/ vmin units)) units)
        (* (ceiling (/ vmax units)) units)
        units
        1))

; (axis-from-spread 21 44 #f)
; (axis-from-spread 0 100 #f)
; (axis-from-spread 0 1 #f)

;(define (axis-from-spread-orig vmin vmax base)
;  "return axis given spread in data and number bass"
;  (let* (;; largest increment (a power of the base) 
;         ;; that's <=  range between vmin and vmax
;         (units (expt base (floor (log (- vmax vmin) base)))) 
;         ;; largest multiple of units <= vmin
;         (axis0 (* (floor (/ vmin units)) units))
;         ;; smallest multiple of unies >= vmax
;         (axis1 (* (ceiling (/ vmax units)) units)))
;    (list axis0 axis1 units 1)))
;(axis-from-spread-orig 1  99 #f)
  
(define (plot-lists data . args)
  (with-optkeys (args yunits)
    (let ((y0 most-positive-fixnum)
          (y1 most-negative-fixnum)
          (x0 0.0)
          (x1 100.0)
          (xaxis #f)
          (yaxis #f)
          (ybase 10)
          (plots (list))
          )
      (if (not (pair? (car data)))
          (set! data (list data)))
      (loop for d in data
            for l = (- (length d) 1)
            do
            (let ((e (loop for y in d
                           for x from 0
                           do
                           (set! y0 (min y0 y))
                           (set! y1 (max y1 y))
                           collect (* x1 (/ x l))
                           collect y)))
              (set! plots (append plots (list :layer e)))
              ))
      (set! xaxis (list 0 100 10 1))
      (set! yaxis (axis-from-spread y0 y1 yunits))
      ;;    (print (list :yaxis yaxis :spec (axis-spec y0 y1 10 ) :data data))
      (apply plot :xaxis xaxis :yaxis yaxis plots)
      )))

;(let* ((n 20)
;       (b (random 64))
;       (foo (loop repeat (between 4 12) collect (+ n (random b))))
;       (bar (loop repeat (between 4 12) collect (+ n (random b)))))
;  (plot-data (list foo bar) )
;  )

(define (plot-envelopes envs . args)
  (with-optkeys (args xunits yunits)
    (if (not (pair? (car envs)))
        (set! envs (list envs)))
    (let ((x0 most-positive-fixnum)
          (x1 most-negative-fixnum)
          (y0 most-positive-fixnum)
          (y1 most-negative-fixnum)
          (xa #f)
          (ya #f)
          (plots (list))
          )
      (do ((e envs (cdr e)))
          ((null? e) #f)
        (do ((t (car e) (cddr t)))
            ((null? t) #f)
          (set! x0 (min x0 (car t)))
          (set! x1 (max x1 (car t)))
          (set! y0 (min y0 (cadr t)))
          (set! y1 (max y1 (cadr t))))
        (set! plots (append plots (list :layer (car e))))
        )
      (set! xa (axis-from-spread x0 x1 xunits))
      (set! ya (axis-from-spread y0 y1 yunits))
      ;;(list xa ya plots)
      (apply plot :xaxis xa :yaxis ya plots)
      )))

;(let ((e (loop repeat 2 for n = (between 5 20) collect (ranenv n)))) (plot-envelopes e))

;
;; the main plot function
;

(define (plot . args)
  (let ((global #t)
	(debug #f)
	(title #f)
	(xaxis #f)
	(yaxis #f)
	(fields (list))
	(points #f)
	(fmat #f)
	(plots (list))
	(access (list))
	(style #f))
    (define (setplotstyle pl sty) (set-car! (list-tail pl 1) sty))
    (define (setplotcolor pl col) (set-car! (list-tail pl 2) col))
    (define (setplottitle pl tit) (set-car! (list-tail pl 3) tit))
    (do ((tail args (cdr tail)))
	((null? tail)
	 (set! plots (reverse plots)))
      (case (car tail)
	((:title)
	 (if (null? (cadr tail))
	     (error "missing title value in ~S" args))
	 (if global
	     (set! title (cadr tail))
	     (setplottitle (car plots) (cadr tail)))
	 (set! tail (cdr tail)))
	((:fields)
	 (if (not global)
	     (error "found non-global fields setting in ~S" args))
	 (if (null? (cdr tail))
	     (error "missing fields value in ~S" args))
	 (set! fields (cadr tail))
	 (if (not (pair? fields))
	     (error "fields value ~S is not a list" fields))
	 (if (< (length fields) 2)
	     (error "too few fields in ~S" fields))
	 (set! tail (cdr tail)))
	((:values )
	 (if (not global)
	     (error "found non-global format setting in ~S" args))
	 (if (null? (cdr tail))
	     (error "missing format value in ~S" args))
	 (set! fmat (cadr tail))
	 (set! tail (cdr tail)))
	((:x-axis :xaxis)
	 (let ((axis (if (null? (cdr tail)) 
			 (error "missing xaxis value in ~S" args)
		     (cadr tail))))
	   (if (not global) (error "found non-global xaxis in ~S" axis))
	   ;;(set! axis (if (pair? axis) (cons 'x axis) (list 'x axis)))
	   (set! xaxis axis)
	   (set! tail (cdr tail))))
	((:y-axis :yaxis)
	 (let ((axis (if (null? (cdr tail)) 
			 (error "missing yaxis value in ~S" args)
		     (cadr tail))))
	   (if (not global) 
	       (error "found non-global yaxis in ~S" axis))
	   ;;(set! axis (if (pair? axis) (cons 'y axis) (list 'y axis)))
	   (set! yaxis axis)
	   (set! tail (cdr tail))))
	((:style)
	 (if (null? (cdr tail))
	     (error "missing style value in ~S" args))
	 (cond ((not global)
		(setplotstyle (car plots) (cadr tail)))
	       (else
		(set! style (cadr tail))))
	 (set! tail (cdr tail)))
	((:color)
	 (if (null? (cdr tail))
	     (error "missing color value in ~S" args))
	 (if (not global)
	     (setplotcolor (car plots) (cadr tail))
	     (error "found global color option in ~S" args))
	 (set! tail (cdr tail)))
	((:debug)
	 (if (null? (cdr tail))
	     (error "missing debug value in ~S" args)
	     (set! debug (cadr tail)))
	 (set! tail (cdr tail)))
	((:points :layer)
	 (if (null? (cdr tail)) 
	     (error "missing points value in ~S" args))
	 (if (not (pair? (cadr tail)))
	     (error "~S is not a points list" (cadr tail)))
	 (set! plots (cons (list (cadr tail) style #f #f) plots))
	 (set! global #f)
	 (set! tail (cdr tail)))
	(else
	 (if (pair? (car tail))
	     (let ((pdata (car tail)))
	       ;; plot is ((x y...) style color title)
	       (set! plots (cons (list pdata style #f #f) plots)))
	     (error "~S is not a valid plot setting" (car tail)))
	 (set! global #f) ; all done with global settings
	 ))
      ) ; end do
    
     (let ((port (open-output-string) 
		 ;;(if debug #t (open-output-string))
		 )
	   (text #f))
      (format port "<plot")
      (if title (format port " title=\"~a\"" title))
      (format port ">") ; end <plot>
      (format port "<fields>")
      (cond ((null? fields)
	     ;; insure fields for each specified axis
	     (field->xml port #f xaxis #f 0 fmat)
	     (field->xml port #f yaxis #f 1 fmat))
	    (else
	     (do ((tail fields (cdr tail))
		  (index 0 (+ index 1)))
		 ((null? tail) #f)
               (let ((field (car tail)))
                 (if (pair? field)
                     (let ((name (car field))
                           (axis #f)
                           (default #f))
                       (set! field (cdr field))
                       (if (pair? field)
                           (begin (set! axis (car field)) (set! field (cdr field))))
                       (if (pair? field)
                           (begin (set! default (car field))))
                       (field->xml port name axis default index fmat))
                     (field->xml port field (list) #f index fmat))))
             ;; create the field access list for points
             (set! access (layer-field-access xaxis yaxis fields))
             ;; dont pass over a default access order
             (if (or (equal? access '(0 1))
                     (equal? access '(0 1 2))
                     (equal? access '(0 1 2 3)))
                 (set! access (list)))))
      (format port "</fields>")
      (format port "<layers>")
      (cond 
       ((null? plots)
	(points->xml port (list (list) style #f #f) fmat access)
	)
       (else
	(unless fmat (set! fmat (guess-data-format (caar plots))))
	(do ((tail plots (cdr tail)))
	    ((null? tail) #f)
	  (points->xml port (car tail) fmat access))))
      (format port "</layers>")
      (format port "</plot>")
      (set! text  (get-output-string port))
      (close-output-port port)
      (cond ((not debug)
	     (ffi_plot_xml text)
	     (void))
	    (else
	     text
	     )))))
; (plot )
; (plot :x-axis '(0 123) '(0 0 1 1) )
; (plot :style "lineandpoint" :x-axis '(0 123) '(0 0 1 1) )
; (plot :style "histogram" :x-axis '(0 123) '(0 0 1 1) :style "line"  '(0 0 2 2) :style "point" :color "red")

(define (layer-field-access xaxis yaxis fields)
  ;; return a list of (upto) four indexes (<x> <y> <x2> <y2>) that
  ;; determine the data fields to acess in the layer's points.
  (define (fieldpos name fields)
    ;; return the field's index from its name
    (if (not (string? name))
        (set! name (symbol->string name)))
    (do ((tail fields (cdr tail))
	 (pos 0 (+ pos 1))
	 (flag #f))
	((or flag (null? tail)) flag)
      (let* ((f (if (pair? (car tail)) (caar tail) (car tail)))
             (n (if (not (string? f)) (symbol->string f) f)))
      (if (string=? n name)
	  (set! flag pos)))))
  (cond ((not xaxis)
	 (cond ((not yaxis)
		'(0 1))
	       ((pair? yaxis)
		(list 0 (fieldpos (car yaxis) fields)
		      (fieldpos (cadr yaxis) fields)))
	       (else
		(list 0 (fieldpos yaxis fields)))))
	((pair? xaxis)
	 (cond ((not yaxis)
		(list (fieldpos (car xaxis) fields)
		      1 (fieldpos (cadr xaxis) fields)))
	       ((pair? yaxis)
		(list (fieldpos (car xaxis) fields)
		      (fieldpos (car yaxis) fields)
		      (fieldpos (cadr xaxis) fields)
		      (fieldpos (cadr yaxis) fields)))
	       (else
		(list (fieldpos (car xaxis) fields)
		      (fieldpos yaxis fields)
		      (fieldpos (cadr xaxis) fields)))))
	(else
	 (cond ((not yaxis)
		(list (fieldpos xaxis fields) 1))
	       ((pair? yaxis)
		(list (fieldpos xaxis fields)
		      (fieldpos (car yaxis) fields)
		      (fieldpos xaxis fields)
		      (fieldpos (cadr yaxis) fields)))
	       (else
		(list (fieldpos yaxis fields)
		      (fieldpos yaxis fields)))))))

(define (field->xml port name axisinfo default-value indx fmat)
  (define (tostring x)
    (cond ((string? x) x)
	  ((keyword? x) (keyword->string x))
	  (else (format #f "~a" x))))
  (if (not name) 
      (set! name (format #f "~c" (integer->char (+ 97 (modulo (+ 23 indx) 26))))))
  (cond ((pair? axisinfo)
	 (format port "<field name=\"~a\"" (tostring name))
	 (do ((tail axisinfo (cdr tail))
	      (delim " axis=\"" " "))
	     ((null? tail) 
              (format port "\""))
	   (format port delim)
	   (format port (tostring (car tail)) )))
	(axisinfo
	 (format port "<field name=\"~a\" axis=\"~a\""
		 (tostring name)
		 (tostring axisinfo) ))
	((= indx 0)
	 ;; insure x field even if not specied. if only y data
	 ;; specified set x axis to ordinal
	 (if (member fmat '(:y y))
	     (format port "<field name=\"~a\" axis=\"ordinal\"" 
		     (tostring name))
	     (format port "<field name=\"~a\" axis=\"unspecified\""
		     (tostring name))))
	((= indx 1)
	 ;; insure y field even if not specified
	 (format port "<field name=\"~a\" axis=\"unspecified\""
		 (tostring name)))
	(else
	 ;; ignore unspecified z axis. shouldn't happen now!
	 ))
  (if default-value
      (format port " default=\"~A\"" (tostring default-value)))
  (format port "/>")
  (void))

; (axis->xml #t 'x)
; (axis->xml #t '(z x))
; (axis->xml #t '(x 0 1))
; (axis->xml #t '(x 1))
; (axis->xml #t '(x 1 0))
; (axis->xml #t '(x -1 1 .5))
; (axis->xml #t '(x 10 1000 -2))

(define (points->xml port plot fmat access)
  ;; plot = (pointdata ptype style color)
  (let ((data (car plot))
	(styl (cadr plot))
	(colr (caddr plot))
	(titl (cadddr plot)))
    (define (tostring x)
      (if (keyword? x) (keyword->string x) (format #f "~a" x)))	    
    (format port "<points")
    (if styl
	(format port " style=\"~a\"" (tostring styl))
	(if (equal? fmat #:y)
	    (format port " style=\"impulses\""))
	)
    (if colr (format port " color=\"~a\"" (tostring colr)))
    (if titl (format port " title=\"~a\"" (tostring titl)))
    (if (not (null? access))
	(do ((tail access (cdr tail))
	     (delim " access=\"" " " ))
	    ((null? tail) (format port "\"" ))
	  (format port delim )
	  (format port "~a" (car tail) )))
    (format port ">")
    (cond ((null? data) ; an empty layer
	   )
	  ((pair? (car data))
	   ;; each sublist is a point record (single line )
	   (do ((tail data (cdr tail)))
	       ((null? tail) #f)
	     (format port "<point>")
	     (do ((e (car tail) (cdr e))
		  (d "" " "))
		 ((null? e) #f)
	       (format port d)
	       (format port "~s" (car e)))
	     (format port "</point>")))
	  ((member fmat '(#:y y))
	   (do ((num 0 (+ num 1))
		(tail data (cdr tail)))
	       ((null? tail) #f)
	     (format port "<point>~s ~s</point>" num (car tail))))
	  ((member fmat '(#:xy xy))
	   (do ((tail data (cddr tail)))
	       ((null? tail) #f)
	     (format port "<point>~s ~s</point>" (car tail) (cadr tail))))
	  (else
	   (error "~S is an illegal point format" fmat)
	   )
	  )
    (format port "</points>")))

;; (plot->xml #t '((.1 .3 .4) #:y "lineandpoint" #f) )
;; (plot->xml #t '((0 .1 .5 -.3 1 .4) #:xy #f "red") )
;; (plot->xml #t '((.1 .3 .4) #:y "lineandpoint" #f) )
;; (plot->xml #t '((0 .1 .9 .5 .3 .9 1 .4 .9) 3 "box" "red") )

(define (guess-data-format dat)
  (let ((a (car dat)))
    (cond ((number? a)
	   (do ((l dat (cdr l))
		(i 0 (+ i 1))
		(x (+ most-negative-fixnum 1)) ; 7 bug!
		(f #f))
	       ((or (null? l) f)
		(if (not f)
		    (if (even? i) #:xy)
		    f))
	     (if (even? i) 
		 (if (>= (car l) x)
		     (set! x (car l))
		     (set! f #:y)))))
	  ((pair? a) #:record)
	  (else 
	   (error "~S is an illegal point format" dat)
	   ))))

; (begin (define foo '(0 0 25 .9 75 .9 100 0)) (define bar (loop repeat 6 collect (random 1.0))))
; (guess-data-format foo)
; (guess-data-format bar)
; (guess-data-format (list '(0 0)))

(define (plot-data title . arg)
  ;; #f = focus #t = all num = layer number
  (let ((layer (if (null? arg) -1
		   (if (eq? (car arg) #f) -1
		       (if (eq? (car arg) #t) -2
			   (if (number? arg) arg
			       (error "plot layer ~S not #f #t or number" 
				      (car arg))))))))
    (let ((str (ffi_plot_data title layer)))
      (read-from-string str))
    ))
  
;;
;; Plot hooks. a plot hook is a function of (at least) two args, it is
;; passed the mousedown X and Y and any additional user args. it
;; should return list of point records or ().
;;

(define *plot-hooks* (make-equal-hash-table))

(define (plot-hook title func . args)
  ;; associate a plot hook with a window title
  (unless (string? title) 
    (error "~S is not a title string" title))
  (unless (or (not func) (procedure? func))
    (error "~S is not a function or #f" func))
  (if func (hash-set! *plot-hooks* title (cons func args))
      (hash-set! *plot-hooks* title #f))
  func)

(define (call-plot-hook title x y)
  ;; call user's hook registered under title, send generated points back
  ;; the plot.
  (define (point? x)
    (and (pair? x) 
	 (not (null? (cdr x)))
	 (every? (lambda (x)
		   (and (number? x)
			(or (integer? x) (inexact? x))))
		 x)))
  (let* ((hook (or (hash-ref *plot-hooks* title)
		   (error "no hook registered for plot ~S"
			  title)))
	 ;; call user function, pass x and y and any user args
	 (data (apply (car hook) x y (cdr hook)))
	 (port #f))
    (cond ((null? data) #f)
	  ((point? data)
	   (set! data (list data)))
	  ((and (pair? data) (every? point? data)) #f)
	  ((not data) 
	   (set! data '()))
	  (else
	   (error "~S is not a list of point records" data)))
    (if (null? data) 
	(void)
	(let ((port (open-output-string))
	      (text #f))
	  (format port "<points>")
	  (do ((tail data (cdr tail)))
	      ((null? tail) #f )
	    (format port "<point>")
	    (do ((e (car tail) (cdr e))
		 (d "" " "))
		((null? e) #f)
	      (format port d)
	      (format port "~s" (car e)))
	    (format port "</point>"))
	  (format port "</points>")
	  (set! text (get-output-string port))
	  (close-output-port port)
	  (ffi_plot_add_xml_points title text)
	  (void)))))
  
; (define (foo x y) (list (list x y) (list (+ x 1) (+ y 1))))
; (plot-hook "Untitled" foo)
; (call-plot-hook "Untitled" 0 0)
; (define (bar x y r i)
;   (let ((spec (fm-spectrum (hertz y) r i)))
;     (loop for k in (spectrum-keys spec :unique #t)
; 	  collect (list x k))))
; (plot-hook "Untitled" bar pi 4)    


