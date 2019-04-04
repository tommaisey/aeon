;; This file will serve as a nursery for SuperCollider SynthDefs and related
;; things like sample management. It's not a library, so can be loaded in
;; its entirety to update definitions live.

;; A useful test function
(define (sine-perc freq length)
  (play-now "sine-grain"
	    (cons ":freq" freq)
	    (cons ":sustain" length)))

(define (test-synth name freq length)
  (play-now name
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

;;-------------------------------------------------------------------
;; Some helper tools for writing synthdefs more clearly.
(define (make-env-gen magnitude env action)
  (env-gen kr 1 magnitude 0 1 action env))

;; Really should get into optional arguments to DRY these.
(define make-asr
  (case-lambda
    ((mag atk sus rel) (make-asr mag atk sus rel -4))
    ((mag atk sus rel crv) (make-asr mag atk sus rel crv remove-synth))
    ((mag atk sus rel crv action)
     (let ([shape (env-linen atk sus rel 1 (repeat crv 4))])
       (make-env-gen mag shape action)))))

(define (make-rand-lfo mag time)
  (mul-add (lfd-noise1 ar time) 1 mag))

(define (make-bus-out bus sig pan)
  (out bus (pan2 sig (mul-add pan 2 -1) 1)))

(define (make-standard-out sig pan)
  (make-bus-out 0 sig pan))

(define (clamp-cutoff cutoff)
  (clip cutoff 30 19500))

;; N-arity versions of rsc3 mul, sub & add. Can DRY these once I
;; understand macro composition better...
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
       expr rest ...))))

;;-------------------------------------------------------------------
;; The synthdef nursery - quite bare at the moment!
(send-synth sc3 "sine-grain"
  (letc ([:freq 440] [:attack 0.01] [:sustain 1]
	 [:amp 0.5] [:pan 0.5])
    
    (let* ([osc (sin-osc ar (lag :freq 0.05) 0)]
	   [env (env-perc :attack :sustain 1 (repeat -4 2))]
	   [env (make-env-gen :amp env remove-synth)]
	   [sig (mul env osc)])
      (make-standard-out sig :pan))))

(send-synth sc3 "sampler"
  (letc ([:attack 0.01] [:sustain 1] [:release 0.25]
	 [:sample 0] [:speed 1]
	 [:amp 0.3] [:pan 0.5])
    
    (let* ([rate (mul :speed (buf-rate-scale kr :sample))]
	   [osc  (play-buf 1 ar :sample rate 1 0 no-loop remove-synth)]
	   [env  (make-asr :amp :attack :sustain :release -4)]
	   [sig (mul env osc)])
      (make-standard-out sig :pan))))

(send-synth sc3 "swirly-keys"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
	 [:freq 440] [:amp 0.2] [:pan 0.5]
	 [:resonance 0.1] [:cutoff 3])
    
    (let* ([:freq (lag :freq 0.1)]
	   [:pan  (lag :pan 0.15)]
	   [:amp (mul :amp (lin-exp :freq 20 10000 1.0 0.2))]
	   [osc1 (lf-saw ar (mul :freq 0.9946) 0)]
	   [osc2 (lf-saw ar (mul :freq 1.0024) 0.3)]
	   [amp-env (make-asr :amp :attack :sustain :release -4)]
	   [cut-env (make-asr :cutoff :attack :sustain :release 2 do-nothing)]
	   [cutoff (clamp-cutoff (add :freq (mul :freq cut-env)))]
	   [flt (moog-ff (add osc1 osc2) cutoff :resonance 0.0)]
	   [sig (mul amp-env flt)])
      (make-standard-out sig :pan))))

(send-synth sc3 "phase-tris"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
	 [:freq 440] [:amp 0.2] [:pan 0.5]
	 [:resonance 0.1])

    (define (make-osc mag freq-mul time)
      (lf-tri ar (make-rand-lfo (mul-n :freq freq-mul mag) time) 0))
    
    (let* ([osc1 (make-osc 0.010 0.5 1/2)]
	   [osc2 (make-osc 0.008 1.0 1)]
	   [osc3 (make-osc 0.002 1.5 2)]
	   [env (make-asr :amp :attack :sustain :release -4)]
	   [sig (mul env (add-n osc1 osc2 osc3))])
      (make-standard-out sig :pan))))

(send-synth sc3 "dual-lopass"
  (letc ([:attack 2.0] [:sustain 1] [:release 1]
	 [:freq 440] [:amp 0.2] [:pan 0.5]
	 [:resonance 0.1] [:cutoff1 1.0] [:cutoff2 2.0])
    
    (define (make-osc freq flt-atk cutoff-prop)
      (let* ([pulse-lfo (sin-osc kr 0.5 0)]
	     [osc (lf-pulse ar freq 0 (add 0.50 (mul 0.35 pulse-lfo)))]
	     [sub (lf-pulse ar (mul 0.5 freq) 0.4 (add 0.50 (mul 0.15 pulse-lfo)))]
	     [osc-flt-env (make-asr 1 flt-atk :sustain :release 1 do-nothing)]
	     [osc-flt-env (add (mul 4 freq) (mul-n 4 freq cutoff-prop osc-flt-env))]
	     [osc-flt-env (clamp-cutoff osc-flt-env)])
	(moog-ff (add osc (mul sub 0.8)) osc-flt-env :resonance 0)))

    (let* ([:freq (lag :freq 0.15)]
	   [:pan (lag :pan 0.15)]
	   [:cutoff1 (lag :cutoff1 0.5)]
	   [:cutoff2 (lag :cutoff2 0.5)]
	   [:amp (mul :amp (lin-exp :freq 20 10000 1.0 0.2))]
	   [osc1 (make-osc (mul :freq 0.5) (mul :attack 2) :cutoff1)]
	   [osc2 (make-osc :freq (mul :attack 1.5) :cutoff2)]
	   [amp-env (make-asr :amp :attack :sustain :release -4)]
	   [sig (mul (add osc1 osc2) amp-env)])
      (make-standard-out sig :pan))))

;;-----------------------------------------------------------------------
;; Events with :group will be added to the node group with the value
;; as its ID. Events with :control must also have :group - control properties
;; will be set on all synths in that node group instead of creating new synths.
(define :group ':group)
(define :control ':control)

;; Time-based properties that will be preprocessed to be relative to the tempo.
;; They are paired with #f so this list can be used directly with alist-get-multi.
(define tempo-dependent-keys
  (list (cons :sustain #f) (cons :attack #f) (cons :release #f)))
