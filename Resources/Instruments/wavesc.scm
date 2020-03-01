;; An SC equivalent of the (wave ...) instrument in SndLib.  This
;; function tries to be speedy and efficient by avoiding intermediate
;; consing (appending) and minimizing message size by only adding
;; values explicitly provided by the caller.

(define* (sc:wave (time 0) dur freq amp (ampenv '(0 0 1 1 75 1 100 0)) degree skew)
  (let* ((pars (list "/s_new" "wavesc" -1 0 1))
         (pts1 (make-sc-env ampenv))
         (env1 (list "/n_setn" -1 "env" (length pts1) ))
         (tail (cddddr pars))) ; pointer to last cons cell
    ;; add args "in place" without copying or traversing
    (if dur (begin (set-cdr! tail (list "dur" dur)) (set! tail (cddr tail))))
    (if freq (begin (set-cdr! tail (list "freq" freq)) (set! tail (cddr tail))))
    (if amp (begin (set-cdr! tail (list "amp" amp)) (set! tail (cddr tail))))
    (if degree (begin (set-cdr! tail (list "pan" (rescale degree 0 90 -1.0 1.0)))
                      (set! tail (cddr tail))))
    ;; add sc ampenv data to envelope message
    (set-cdr! (cdddr env1) pts1)
    ;; if there is no skew envelope use the basic "wavesc" instrument
    (if (not skew)
        (osc:bundle time pars env1)
        ;; else use the "wavesk" synth and add skew env to bundle
        (let* ((pts2 (make-sc-env skew))
               (env2 (list "/n_setn" -1 "skew" (length pts2))))
          ;; replace synth name with skew version
          (set-car! (cdr pars) "wavesk")
          ;; add sc env data to skew envelope message
          (set-cdr! (cdddr env2) pts2)
          (osc:bundle time pars env1 env2)))))

; (osc:open 57121 57110)
; (sc:wave 0)
; (sc:wave 0 :dur .1 :freq (between 220 440))
; (sc:wave 0 :freq 440 :skew (list 0 1 1 .5))
; (sc:wave 0 :dur .5 :freq 440 :skew (list 0 1 1 1.059))


