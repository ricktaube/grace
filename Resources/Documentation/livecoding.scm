;;; -*- syntax: Lisp; font-size: 16; theme: "Emacs"; -*-

;
;; Live Coding  (Halim Beere, halimbeere@gmail.com)
;

; To do live coding, two things are needed.  (1) Sprouting processes
; on the fly and having them sync with other processes already running.
; To do this, see the Metronomes example file.
; And (2) updating an already running process with new behavior.

; By default, Grace will allow you to sprout a single process as many
; times as you like.  This is by design.  Define the following
; process, tick-tock().

(define (tick-tock )
  (process 
    with transp = (between -20 20)
    for x from 0
    do
    (if (= (mod x 2) 0)
      (mp:midi :key (+ 72 transp) :dur (metro-dur 0.5))
      (mp:midi :key (+ (pick '(84 82 80 79)) transp) :dur (metro-dur 0.5)))
    (wait .5)))


; now sprout it several times.  You'll notice that
; several of them will run simultaneously.

(sprout (tick-tock ) (sync ))

; stop all processes

(stop )

; behind the scenes each of these processes actually has a default
; id number.  The default is 0, and Grace does not limit the
; the number of processes that can exist with id 0.  This is ONLY
; true of the default id.

; Let's sprout tick-tock() with its own unique id.

(sprout (tick-tock ) (sync ) 1)

; Now, if we try to sprout tick-tock() AGAIN with the same id (which
; is given above as the number 1), then the new process will replace
; the old process.  Evaluate the next line several times.

(sprout (tick-tock ) :id 1)

(stop )

; For any id (except the default id of 0), Grace will only allow ONE
; process to exist with that id.  If you try to sprout another process
; with that same id, the old process will be replaced with the new one.
; This will occur even if the processes have different names.  The name
; doesn't matter.  Only the id number.

; For instance, define the below process.

(define (hymn )
  (process 
    with subpat = (make-cycle '(2 1.5 1 .5))
    with pat = (make-cycle (list 2 2 1 subpat))
    for rhy = (next pat)
    do
    (mp:midi :key (+ 63 12) :dur (metro-dur rhy))
    (mp:midi :key 63 :dur (metro-dur rhy))
    (mp:midi :key (- (pick '(67 68 70 72)) 12) :dur (metro-dur rhy))
    (mp:midi :key (+ (pick '(67 68 70 72)) 12) :dur (metro-dur rhy))
    (wait rhy)))

; sprout it with id 1.

(sprout (hymn ) (sync ) :id 1)

; sprout tick-tock with id 1.

(sprout (tick-tock ) (sync ) :id 1)

; Go back and forth above, and notice that each process will wait until
; the other is finished before replacing it. The replace happens
; in time, in other words.

; Sprout with a different id.  Choose one below

(sprout (hymn ) (sync ) :id 2)

(sprout (tick-tock ) (sync ) :id 2)

; Now you have two processes running, each with a different id.
; You can selectively stop a process by indicating its id, and stop
; will not touch the other running processes.

(stop 1)

(stop 2)

; Try this again, but first let's sprout using three custom ids, then 
; a few using the default id.


(sprout (tick-tock ) (sync ) :id 1)

(sprout (tick-tock ) (sync ) :id 2)

(sprout (tick-tock ) (sync ) :id 3)

(sprout (tick-tock ) (sync ))

(sprout (tick-tock ) (sync ))

; Like above, we can stop just the process with id = 1.

(stop 1)

; But we can also stop ONLY the processes that have default ids.
; Recall that the default id is 0, so we call:

(stop 0)


; Now only processes id = 2 and id = 3 are running.
; Calling stop() with no arguments will stop all running processes,
; regardless of their ids.

(stop )

; note that ids can also be strings, if desired.  note that strings
; are converted to numbers behind the scenes, so there is a slight
; chance that a custom numbered id you choose could conflict with
; the id of a string once converted to a number. For example,
; an id of "bass" will become id number 3016415.

(string-hash "bass")

; Of course, it is unlikely that you will choose 3016415 as your id,
; but if you ever have strange behavior when using ids, you may
; want to check to see if your string ids are conflicting with your
; numbered ids.

; both sprout and stop can handle strings.

(sprout (tick-tock ) :id "soprano")

; replace it

(sprout (tick-tock ) :id "soprano")

; stop it

(stop "soprano")

; strings are supported to help ids be more memorable and descriptive.
; this may help reduce confusion while live coding!

(sprout (hymn ) :id "hymn")

(stop "hymn")

; Of course, one of the main reasons to do this is to allow a user
; to update an existing process with new behavior.  Let's do exactly
; that.

; Define the process again, slightly different from above.

(define (tick-tock )
  (process 
    for x from 0
    do
    (if (= (mod x 2) 0)
      (mp:midi :key 72 :dur (metro-dur .5))
      (mp:midi :key (pick '(84 82 80 79)) :dur (metro-dur 0.5)))
    (wait .5)))

; sprout it.

(sprout (tick-tock ) (sync ) :id "tick")

; We could simply alter the code above and re-define the process, but
; the code is duplicated and altered below for illustration purposes.
; Notice that we changed the rhythm of the process.

(define (tick-tock )
  (process
    with rhy-pat = (make-cycle '(.5 .25 .25))
    for x from 0
    for dur = (next rhy-pat)
    do 
    (if (= (mod x 2) 0)
      (mp:midi :key 72 :dur (metro-dur dur))
      (mp:midi :key (pick '(84 82 80 79)) :dur (metro-dur dur)))
    (wait dur)))

; Now that we have redefined our process, let's sprout it again with
; the same id -> "tick"

(sprout (tick-tock ) :id "tick")

; And one more time, let's alter the process and re-sprout it.

(define (tick-tock )
  (process 
    with rhy-pat = (make-cycle '(.25 .25 .125 .125 .125 .125))
    with root = 72
    for x from 0
    for dur = (next rhy-pat)
    do 
    (if (= (mod x 10) 0)
      (set! root (+ (pick '(72 75 79)) (pick '(-24 -12 0))))
      (set! dur (pick '(.25 .5))))
    (if (= (mod x 2) 0)
      (mp:midi :key root :dur (* (metro-dur dur) 2))
      (mp:midi :key (+ (pick '(84 82 80 79)) (pick '(-24 -12 0))) :dur (metro-dur dur)))
    (wait dur)))

(sprout (tick-tock ) (sync ) :id "tick")

; sprout another version on top with a new id, using to the metronome
; to keep it in sync

(sprout (tick-tock ) (sync ) :id "tock")

; lastly, sprout our old hymn() process with a default id.

(sprout (hymn ) (sync ))

; stop the first process

(stop "tick")

; stop the second process

(stop "tock")

; stop the default process

(stop 0)
