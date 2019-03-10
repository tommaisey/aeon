;; -*- geiser-scheme-implementation: chez-*-
(load "libs.scm")

;;-----------------------------------------------------------------
;; Some SuperCollider setup: a default server, synthdef & event sender.
;; TODO: blocks if SuperCollider isn't running!! 
(define sc3 (udp:open "127.0.0.1" 57110))
(send sc3 (g-new1 1 add-to-tail 0))

(define (play-when name t . arg-pairs)
  (let ([args (list (s-new0 name -1 add-to-tail 1)
		    (n-set -1 arg-pairs))])
    (send sc3 (bundle t args))))

(define (play-now name . arg-pairs)
  (play-when name (utc) arg-pairs))

;; Override this rsc3 macro (which defines synthdef args) to also make
;; a top level 'keyword' (i.e. self-evaluating symbol) out of the name.
(define-syntax letc
  (syntax-rules ()
    ((_ () expr)
     expr)
    ((_ ((name default) ...) expr)
     (let ((name (begin
		   (define-top-level-value (quote name) (quote name))
		   (make-control* (symbol->string (quote name)) default kr 0)))
	   ...)
       expr))))

;; Extra keyword definitions
(define :inst ':inst)

(send-synth
 sc3 "sine-grain"
 (letc ([:freq 440] [:attack 0.01] [:sustain 1] [:amp 0.2] [:pan 0.5])
   (let* ([osc (sin-osc ar :freq 0)]
	  [env (env-perc :attack :sustain 1 (list -4 -4))]
	  [env (env-gen kr 1 :amp 0 1 remove-synth env)])
     (out 0 (pan2 (mul env osc)
		  (add (mul :pan 2) -1) 1)))))

(send-synth
 sc3 "sampler"
 (letc ([:sample 0] [:attack 0.01] [:sustain 1] [:pan 0.5] [:speed 1] [:release 0.25] [:amp 0.3])
   (let* ([rte (mul :speed (buf-rate-scale kr :sample))]
	  [osc (play-buf 1 ar :sample rte 1 0 no-loop remove-synth)]
	  [env (env-linen :attack :sustain :release 1 '(-4 -4 -4))]
	  [env (env-gen kr 1 :amp 0 1 remove-synth env)])
     (out 0 (pan2 (mul env osc)
		  (add (mul :pan 2) -1) 1)))))

(define (add-sample path bufnum)
  (async sc3 (b-alloc-read bufnum path 0 0)))

(define (check-sample bufnum)
  (async sc3 (b-query1 bufnum)))

(define (sine-perc freq length)
  (play-now "sine-grain"
	    (cons ":freq" freq)
	    (cons ":length" length)))

;;-----------------------------------------------------------------
;; Plays a event at the right time in the future.
;; TODO: adjust latency based on frame jitter
(define (play-event event current-beat)
  (define (entry-convert pair)
    (cons (symbol->string (car pair)) (cdr pair)))
  (let* ([event (preprocess-event event)]
	 [inst (event-get event :inst "sine-grain")]
	 [beat (event-beat event)]
	 [until (secs-until beat current-beat bpm)]
	 [t (+ (utc) until playback-latency)])
    (apply play-when inst t (map entry-convert (event-clean event)))))

;; Preprocess an event. Computes frequency for events using the harmony
;; system. Gives sampler instrument to events with samples. 
(define (preprocess-event event)
  (cond
   ((event-get event :sample #f)
    (event-set event :inst (event-get event :inst "sampler")))
   (else
    (event-set event :freq (event-freq event)))))

;; Our test pattern for the moment. Redefine for fun and profit!
(define p1 (in+ 1))
(define (reset-p1) (define-top-level-value 'p1 (in+ 1)))

;; Adds custom priting of contexts.
(record-writer (type-descriptor context) context-print) 

;;-----------------------------------------------------------------
;; A thread that wakes up every playback-chunk beats to call (process-chunk)
;; This essentially controls playback for now.
(define bpm 105)
(define playback-thread #f)
(define playback-chunk 1/4) ; 1 quarter measure for now
(define playback-thread-semaphore (make-semaphore))
(define playback-time 0)
(define playback-latency 0.2)

;; Called each chunk of time by the playback thread.  
(define (process-chunk)
  (guard (x [else (handle-error x)])
    (let* ([t1 playback-time]
	   [t2 (+ t1 playback-chunk)]
	   [c (context-trim (render-arc p1 (make-arc t1 t2)))]
	   [events (context-events-next c)])
      (for-each (lambda (n) (play-event n t1)) events)
      (set! playback-time t2))))

;; Only creates new thread if one isn't already in playback-thread.
(define (start-thread sem)
  (when (not playback-thread)
    (set! playback-thread
      (start-suspendable-thread
       process-chunk (* playback-chunk (bpm->spm bpm)) sem))))

(define (start)
  (start-thread playback-thread-semaphore)
  (stop-waiting playback-thread-semaphore))

(define (pause)
  (start-waiting playback-thread-semaphore))

(define (stop)
  (pause)
  (set! playback-time 0))

(define (handle-error condition)
  (let ([p (console-output-port)])
    (display-condition condition p)
    (newline p)
    (flush-output-port p)
    (reset-p1)))

;;------------------------------------------------------------------
;; Set up some inital drum samples
(begin
  (define drum-dir
  "/Users/tommaisey/Dropbox/Music Production/Samples & Patches/Drum Samples/")
  (define (drum-path file)
    (string-append drum-dir file))
  (define (tape-drum file)
    (string-append drum-dir "SM43 - Vinyl and Tape Drum Hits/" file))
  (define (mbase-kik file)
    (string-append drum-dir
		   "WA_Drum_Machine_Collection/WA_Drum Machines_01/"
		   "WA_Airbase Drums/kick drums/mbase 11/" file))
 
  (begin (define bd 1) (add-sample (mbase-kik "waad_mbase_kick_120.wav") 1))
  (begin (define sn 2) (add-sample (tape-drum "tape/snares/machine/SM43_vth_tpe_snr_mch_slammin.wav") 2))
  (begin (define hh 3) (add-sample (tape-drum "tape/hi-hats/closed/SM43_vth_tpe_hh_clsd_hitin.wav") 3))
  (begin (define oh 4) (add-sample (tape-drum "tape/hi-hats/open/SM43_vth_tpe_hh_op_itsgood.wav") 4))
  (begin (define xt 5) (add-sample (drum-path "Cross Sticks/82280__kevoy__acoustic-side-stick.wav") 5)))
