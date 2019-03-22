;; This file will serve as a nursery for SuperCollider SynthDefs and related
;; things like sample management. It's not a library, so can be loaded in
;; its entirety to update definitions live.

;; A useful test function
(define (sine-perc freq length)
  (play-now "sine-grain"
	    (cons ":freq" freq)
	    (cons ":sustain" length)))

;;------------------------------------------------------------------
;; Set up some inital drum samples

(define (add-sample path bufnum)
  (async sc3 (b-alloc-read bufnum path 0 0)))

(define (check-sample bufnum)
  (async sc3 (b-query1 bufnum)))

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

;;------------------------------------------------------------------
;; Extra keyword definitions
(define :inst ':inst)
(define :bps ':bps) ;; beats per second, for tempo syncing

;;-------------------------------------------------------------------
;; Some helper tools for writing synthdefs more clearly.
(define (make-env-gen magnitude env action)
  (env-gen kr 1 magnitude 0 1 action env))

;; Really should get into optional arguments to DRY these.
(define make-linen
  (case-lambda
    ((mag atk sus rel) (make-linen mag atk sus rel -4))
    ((mag atk sus rel crv) (make-linen mag atk sus rel crv remove-synth))
    ((mag atk sus rel crv action)
     (let ([shape (env-linen atk sus rel 1 (repeat crv 4))])
       (make-env-gen mag shape action)))))

(define (make-rand-lfo mag time)
  (mul-add (lfd-noise1 ar time) 1 mag))

;; N-arity versions of rsc3 mul, sub & add. Can DRY these once I understand
;; macro composition better...
(define-syntax mul-n
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (mul x (mul-n y ...)))))

(define-syntax sub-n
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (sub x (sub-n y ...)))))

(define-syntax add-n
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (add x (add-n y ...)))))

;; Override this rsc3 macro (which defines synthdef args) to also make
;; a top level 'keyword' (i.e. self-evaluating symbol) out of the name.
(define-syntax letc
  (syntax-rules ()
    ((_ () expr rest ...)
     (begin expr rest ...))
    
    ((_ ((name default) ...) expr rest ...)
     (let ((name (begin
		   (define-top-level-value (quote name) (quote name))
		   (make-control* (symbol->string (quote name)) default kr 0)))
	   ...)
       expr
       rest ...))))

;;-------------------------------------------------------------------
;; The synthdef nursery - quite bare at the moment!
(send-synth
 sc3 "sine-grain"
 (letc ([:freq 440] [:attack 0.01] [:sustain 1] [:amp 0.2] [:pan 0.5])
   
   (let* ([osc (sin-osc ar :freq 0)]
	  [env (env-perc :attack :sustain 1 (repeat -4 2))]
	  [env (make-env-gen :amp env remove-synth)])
     (out 0 (pan2 (mul env osc)
		  (mul-add :pan 2 -1) 1)))))

(send-synth
 sc3 "sampler"
 (letc ([:sample 0] [:attack 0.01] [:sustain 1] [:pan 0.5]
	[:speed 1] [:release 0.25] [:amp 0.3])
   
   (let* ([rte (mul :speed (buf-rate-scale kr :sample))]
	  [osc (play-buf 1 ar :sample rte 1 0 no-loop remove-synth)]
	  [env (make-linen :amp :attack :sustain :release -4)])
     (out 0 (pan2 (mul env osc)
		  (mul-add :pan 2 -1) 1)))))
(send-synth
 sc3 "swirly-keys"
 (letc ([:freq 440] [:attack 0.4] [:sustain 1] [:release 1]
	[:amp 0.2] [:pan 0.5] [:resonance 0.1])
   
   (let* ([osc1 (lf-saw ar (add :freq -0.5) 0)]
	  [osc2 (lf-saw ar (add :freq +1.5) 0.3)]
	  [amp-env (make-linen :amp :attack :sustain :release -4)]
	  [cut-env (make-linen 1 :attack :sustain :release 2 do-nothing)]
	  [flt (moog-ff (add osc1 osc2) (add 300 (mul 2000 cut-env)) :resonance 0)])
     (out 0 (pan2 (mul amp-env flt)
		  (mul-add :pan 2 -1) 1)))))

(send-synth
 sc3 "phase-tris"
 (letc ([:freq 440] [:attack 0.4] [:sustain 1] [:release 1]
	[:amp 0.2] [:pan 0.5] [:resonance 0.1])

   (define (make-osc mag freq-mul time)
     (lf-tri ar (make-rand-lfo (mul-n :freq freq-mul mag) time) 0))
   
   (let* ([osc1 (make-osc 0.010 0.5 1/2)]
	  [osc2 (make-osc 0.008 1.0 1)]
	  [osc3 (make-osc 0.002 1.5 2)]
	  [env (make-linen :amp :attack :sustain :release -4)])
     (out 0 (pan2 (mul env (add (add osc1 osc2) osc3))
		  (mul-add :pan 2 -1) 1)))))

(send-synth
 sc3 "dual-lopass"
 (letc ([:freq 440] [:attack 2.0] [:sustain 1] [:release 1]
	[:amp 0.2] [:pan 0.5] [:resonance 0.1] [:width 0.2])

   (define (make-osc freq flt-atk width-mul)
     (let* ([osc (lf-pulse ar freq 0 0.5)]
	    [osc-flt-env (make-linen 1 flt-atk :sustain :release 1 do-nothing)]
	    [osc-flt-env (add-n freq
				(mul freq width-mul)
				(mul-n 4 osc-flt-env freq))])
       (moog-ff osc osc-flt-env :resonance 0)))

   (let* ([osc1 (make-osc (mul :freq 0.5) :attack 0)]
	  [osc2 (make-osc :freq (mul :attack 1.5) :width)]
	  [amp (mul :amp (lin-lin :freq 20 10000 1.0 0.2))]
	  [amp-env (make-linen amp :attack :sustain :release -4)])
     (out 0 (pan2 (mul (add osc1 osc2) amp-env)
		  (mul-add :pan 2 -1)
		  1)))))

;; Extra overriden functions from rsc3 - I prefer to leave that source as it
;; is for now.
