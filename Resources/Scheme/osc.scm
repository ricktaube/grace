;;; **********************************************************************
;;; Copyright (C) 2009, Rick Taube
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the Lisp Lesser Gnu Public License. The text of
;;; this agreement is available at http://www.cliki.net/LLGPL            
;;; **********************************************************************

(define (osc:open-input port )
  (if (and (integer? port) (> port 0))
      (ffi_osc_open_input port)
      (error "invalid port number: ~S" port)))

(define (osc:close-input )
  (ffi_osc_close_input ))

(define (osc:open-output host . port)
  (cond ((null? port)
	 (set! port host)
	 (set! host "localhost"))
	(else
	 (if (> (length port) 1)
	     (error "too many arguments: ~S" (cons host port))
	     (set! port (car port)))))
  (if (not (string? host))
      (error "host ~S is not a string hostname." host))
  (if (or (not (integer? port)) (< port 1))
      (error "port number ~S is invalid" port))
  (ffi_osc_open_output host port))

(define (osc:close-output )
  (ffi_osc_close_output ))

(define (osc:message path . data)
  (if (null? data)
      (if (string? path)
          (ffi_osc_send_message path data)
          (if (pair? path)
              (if (string? (car path))
                  (ffi_osc_send_message (car path) (cdr path))
                  (error "not an OSC message: ~S" path))
              (error "not an OSC message: ~S" path)))
      (if (string? path)
          (ffi_osc_send_message path data)
          (error "not an OSC path: ~S" path))
      ))

; (osc:message "/hi" 1 2)
; (osc:message '("/hi" 1 2))

; (osc:bundle 100 '("/hi" 1) '("/ho" 2))
; (osc:bundle 100 '(("/hi" 1) ("/ho" 2)))
; (osc:bundle '(100 ("/hi" 1) ("/ho" 2)))
; (osc:bundle '(100 (("/hi" 1) ("/ho" 2))))

;data ( ("/hi" 1) ("/ho" 2)))
;data ( (("/hi" 1) ("/ho" 2))))
;(OSC:BUNDLE 100 (("/hi" 1) ("/ho" 2)))

(define (osc:bundle time . data)
  (if (null? data)
      (if (pair? time)
          (begin (set! data (cdr time))
                 (set! time (car time)))
          (error "not an OSC bundle: ~S" time)))
  (if (number? time)
      (if (pair? (car data)) ; data must be a message or list of messages
          (if (pair? (caar data)) ; messages passed as single list
              (if (null? (cdr data))
                  (ffi_osc_send_bundle time (car data))
                  (error "not an OSC bundle: ~S" (car data)))
              (ffi_osc_send_bundle time data))
          (error "not an OSC message: ~S" (car data)))
      (error "not an OSC time tag: ~S" time)))

(define (osc:receive . args)
  (if (null? args) ; clear all hooks
      (ffi_osc_set_hook "*" #f)
      (let ((arg (car args))
            (rest (cdr args)))
        (if (null? rest) ; set/clear default hook
            (if (not arg) 
                (ffi_osc_set_hook "" #f) ; clear default hook     
                (if (and (procedure? arg)
                         ;(= (car (procedure-arity arg) ) 1)
                         )
                    (ffi_osc_set_hook "" arg)
                    (error "osc:receive: receiver not #f or a procedure of one argument: ~S"
                           arg)))
            ;; rest is ( proc|#f)
            (let ((op arg)
                  (proc (car rest))
                  (rest (cdr rest)))
              (if (null? rest)
                  (if (string? op)
                      (if (or (not proc) ;; clear/set valid hook
                              (and (procedure? proc)
                                   ;(= (car (procedure-arity proc)) 1)
                                   ))
                          (ffi_osc_set_hook op proc)
                          (error "osc:receive: receiver not #f or a procedure of one argument: ~S"
                                 proc))
                      (error "osc:receive: invalid path: ~S" op))
                  (error "osc:receive: too many arguments: ~S" args)))))))

(define (osc:receive? . args)
  (if (null? args)
      (ffi_osc_is_hook "*") ; is any hook set?
      (if (null? (cdr args))
          (let ((path (car args)))
            (if (string? path)
                (ffi_osc_is_hook path)
                (error "osc:receive?: invalid path: ~S" op)))
          (error "osc:receive?: too many arguments: ~S" args))))

