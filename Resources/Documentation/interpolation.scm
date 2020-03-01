;
;; Interpolation
;

; To run code put your cursor after each expression and press
; Command-Return, then check the console window for any output.

; The interp function perform interpolation, it can be called either
; of two ways:

;    interp(x, x1, y1, x2, y2 ...)
;    interp(x env [, scale: s] [, offset: o] [, base: b])

; The first argument is the x value to look up. The other arguments
; are the xy coordinate pairs that define the shape of the
; envelope. The interp function can take either a sequence of x y
; arguments OR a single envelope list. In either case the x values
; must be in increasing order (the y values are unordered). The two
; calls in this example are equivalent.

(interp .5 0 0 1 100)

(interp .5 '(0 0 1 100))

; The next example demonstrates how interp can be used to control
; parameter values in a musical process. The process interpolates
; amplitude and tempo values using two interp expressions. Each
; interpolation divides the total length of the process into two
; segments on either side of a randomly chosen midpoint mid between 20
; and 80 percent of the total length of the process. Each
; interpolation produces increasing values over the first segment and
; decreasing values over the second.

(define (play-interp len rate keyn wid mintem maxtem
                     minamp maxamp)
  (let ((maxx (- len 1))
        (mid (+ (* .2 len) (ran (* .6 len)))))
    (process for x below len
             for a = (interp x 0 minamp mid maxamp maxx minamp)
             for c = (interp x 0 mintem mid maxtem maxx mintem)
             do
             (mp:midi :key (+ keyn (random wid)) :amp a :dur (* rate 1.5 c))
             (wait (* rate c))
             )))

(sprout (play-interp 40 .2 84 7 1 .6 .4 .8))

(sprout (list (play-interp 40 .2 84 7 1 .6 .4 .8)
              (play-interp 40 .2 77 7 1 .6 .4 .8)
              (play-interp 40 .2 70 7 1 .6 .4 .8)
              (play-interp 40 .2 63 7 1 .6 .4 .8)
              (play-interp 40 .2 56 7 1 .6 .4 .8)
              (play-interp 40 .2 49 7 1 .6 .4 .8))
        '(0 1 2 3 4 5 6)
        )

;
;; Normalized Envelopes
;

; The envelope is a very simple yet powerful data structure in
; computer composition because the "shape" it defines can be scaled
; and offset to control the evolution different sound parameters. A
; 'normalized envelope' is an envelope whose xy coordinates are
; defined within the range 0 to 1. Normalized envelopes express
; proportion, or shape, rather than absolute values. Composers often
; define global variables to hold the most common envelopes they work
; with. Some sample normalized envelope definitions:

(define ramp-up '(0 0 1 1))
(define ramp-down '(1 0 0 1))
(define tri-env '(0 0 .5 1 1 0))
(define mid-env '(0 0 .25 1 .75 1 1 0))
(define exp-down '(0 1 .25 1/4 .5 1/16 .75 1/32 1 0))
(define exp-up '(0 0 .25 1/32 .5 1/16 .75 1/4 1 1))

; To use a normalized envelope the y output value must be rescaled and
; offset to lie within the appropriate range for a given
; parameter.

(define (shaper len envl rate amplow amphigh keylow keyhigh)
  (process for i below len
           for x = (/ i (- len 1))
           for y = (interp x envl)
           do
           (mp:midi :dur rate
                    :key (rescale y 0 1 keylow keyhigh)
                    :amp (rescale y 0 1 amplow amphigh))
           (wait rate)
           ))

(sprout (shaper 20 tri-env .15 .9 .1 48 64))

(sprout (shaper 20 exp-down .15 .5 .2 20 30))
