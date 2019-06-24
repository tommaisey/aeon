;; This file will serve as a nursery for SuperCollider SynthDefs and related
;; things like sample management. It's not a library, so can be loaded in
;; its entirety to update definitions live.

;; A useful test function
(define (test-synth name freq length)
  (play-now name
            (cons ":freq" freq)
            (cons ":sustain" length)))

(define (sine-perc freq length)
  (test-synth "sine-grain" freq length))

;;------------------------------------------------------------------
;; Set up some inital drum samples

(define (add-sample path bufnum)
  (async sc3 (b-alloc-read bufnum path 0 0)))

(define (check-sample bufnum)
  (async sc3 (b-query1 bufnum)))

(define bufnum-table
  (make-hashtable string-hash string=? 128))

(define bufnum-counter 0)

(define (get-bufnum file)
  (or (hashtable-ref bufnum-table file #f)
      (begin
        (set! bufnum-counter (+ bufnum-counter 1))
        (hashtable-set! bufnum-table file bufnum-counter)
        (add-sample file bufnum-counter)
        bufnum-counter)))

;;------------------------------------------------------------------
;; Keyword for specifying which synthdef an event applies to.
(declare-keyword :inst)

;; Synths using tempo-based effects can have this property.
;; Its value should be set to the number of seconds per beat.
;; See also tempo-dependent-keys, which are preprocessed by the
;; client to be relative to the tempo.
(declare-keyword :tempo)

;; Events with :group will be added to the node group with the value
;; as its ID. Events with :control must also have :group - control properties
;; will be set on all synths in that node group instead of creating new synths.
(declare-keyword :group)
(declare-keyword :control)

;; If the user supplies a vector of samples, for the :sample key, this key can
;; be used to select between them.
(declare-keyword :sample-idx)

;; Defines some globally reserved bus numbers. These are used for
;; standard effects busses in the synthdefs. The numbers start at
;; 16. Buses are all stereo.
(define (get-bus-num i) (* i 2))
(define :verb1  (get-bus-num 0))
(define :verb2  (get-bus-num 1))
(define :delay1 (get-bus-num 2))
(define :delay2 (get-bus-num 3))

(define (private-bus i)
  (add i (add num-output-buses num-input-buses)))

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

(define (make-pan sig pos)
  (pan2 sig (mul-add pos 2 -1) 1))

(define (make-bus-out bus sig pan)
  (out bus (make-pan sig pan)))

;; Makes N parallel outputs, where N is the number of pairs.
;; each pair should be: (bus-num . send-amt)
(define (make-outputs sig pan . pairs)
  (if (null? pairs)
      (raise "Must supply bus info pairs to make-standard-out")
      (let* ([panned (make-pan sig pan)]
             [make-out (lambda (info) (out (car info) (mul panned (cdr info))))]
             [combine (lambda (ug info) (mrg2 ug (make-out info)))])
        (fold-left combine (make-out (car pairs)) (cdr pairs)))))

;; Synthdefs containing filters should be careful to keep
;; cutoff frequencies in a safe range.
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
                     [:amp 0.5] [:pan 0.5]
                     [:bus1 :verb1]  [:bus1-amt 0]
                     [:bus2 :delay1] [:bus2-amt 0])

    (let* ([osc (sin-osc ar (lag :freq 0.05) 0)]
           [env (env-perc :attack :sustain 1 (repeat -4 2))]
           [env (make-env-gen 1 env remove-synth)]
           [sig (mul env osc)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(send-synth sc3 "sampler"
  (letc ([:attack 0.01] [:sustain 1] [:release 0.25]
                        [:sample 0] [:speed 1]
                        [:amp 0.3] [:pan 0.5]
                        [:resonance 0.0] [:cutoff 1.0]
                        [:sample-pos 0.0]
                        [:bus1 :verb1]  [:bus1-amt 0]
                        [:bus2 :delay1] [:bus2-amt 0])

    (let* ([rate (mul :speed (buf-rate-scale kr :sample))]
           [frames (buf-frames kr :sample)]
           [pos (mul frames :sample-pos)]
           [osc (play-buf 1 ar :sample rate 1 pos no-loop remove-synth)]
           [osc (moog-ff osc (add 75 (mul :cutoff 17000)) :resonance 0)]
           [env (make-asr 1 :attack :sustain :release -4)]
           [sig (mul env osc)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(send-synth sc3 "swirly-keys"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
                       [:freq 440] [:amp 0.2] [:pan 0.5]
                       [:cutoff 3] [:resonance 0.1]
                       [:bus1 :verb1]  [:bus1-amt 0]
                       [:bus2 :delay1] [:bus2-amt 0])

    (let* ([:freq (lag :freq 0.1)]
           [:pan  (lag :pan 0.15)]
           [:amp (mul :amp (lin-exp :freq 20 10000 1.0 0.2))]
           [osc1 (lf-saw ar (mul :freq 0.9946) 0)]
           [osc2 (lf-saw ar (mul :freq 1.0024) 0.3)]
           [amp-env (make-asr 1 :attack :sustain :release -4)]
           [cut-env (make-asr :cutoff :attack :sustain :release 2 do-nothing)]
           [cutoff (clamp-cutoff (add :freq (mul :freq cut-env)))]
           [flt (moog-ff (add osc1 osc2) cutoff :resonance 0.0)]
           [sig (mul amp-env flt)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(send-synth sc3 "phase-tris"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
                       [:freq 440] [:amp 0.2] [:pan 0.5]
                       [:resonance 0.1] [:slop 0.05]
                       [:bus1 :verb1]  [:bus1-amt 0]
                       [:bus2 :delay1] [:bus2-amt 0])

    (define (make-osc mag freq-mul time)
      (let ([freq (mul :freq freq-mul)])
        (lf-tri ar (add freq (make-rand-lfo (mul freq mag) time)) 0)))

    (let* ([osc1 (make-osc (mul :slop 0.05) 0.5 1/2)]
           [osc2 (make-osc (mul :slop 0.04) 1.0 4/3)]
           [osc3 (make-osc (mul :slop 0.01) 1.5 3/2)]
           [env (make-asr 1 :attack :sustain :release -4)]
           [sig (mul env (add-n osc1 osc2 osc3))])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(send-synth sc3 "dual-lopass"
  (letc ([:attack 2.0] [:sustain 1] [:release 1]
                       [:freq 440] [:amp 0.2] [:pan 0.5]
                       [:resonance 0.1] [:cutoff1 1.0] [:cutoff2 2.0]
                       [:bus1 :verb1]  [:bus1-amt 0]
                       [:bus2 :delay1] [:bus2-amt 0])

    (define (make-osc freq flt-atk cutoff-prop)
      (let* ([pulse-lfo (sin-osc kr 0.5 0)]
             [osc (lf-pulse ar freq 0 (add 0.50 (mul 0.35 pulse-lfo)))]
             [sub (lf-pulse ar (mul 0.5 freq) 0.4 (add 0.50 (mul 0.15 pulse-lfo)))]
             [osc-flt-env (make-asr 1 flt-atk :sustain :release 1 do-nothing)]
             [osc-flt-env (add (mul 4 freq) (mul-n 4 freq cutoff-prop osc-flt-env))]
             [osc-flt-env (clamp-cutoff osc-flt-env)])
        (moog-ff (add osc (mul sub 0.8)) osc-flt-env :resonance 0)))

    (let* ([:freq (lag :freq 0.175)]
           [:pan (lag :pan 0.15)]
           [:cutoff1 (lag :cutoff1 0.5)]
           [:cutoff2 (lag :cutoff2 0.5)]
           [:amp (mul :amp (lin-exp :freq 20 10000 1.0 0.2))]
           [osc1 (make-osc (mul :freq 0.5) (mul :attack 2) :cutoff1)]
           [osc2 (make-osc :freq (mul :attack 1.5) :cutoff2)]
           [amp-env (make-asr 1 :attack :sustain :release -4)]
           [sig (mul (add osc1 osc2) amp-env)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

;; Time-based properties that will be preprocessed to be relative to the tempo.
;; They are paired with #f so this list can be used directly with alist-get-multi.
(define tempo-dependent-keys
  (make-alist :sustain #f :attack #f :release #f))

;;-----------------------------------------------------------------------
;; Bus effects

(send-synth sc3 "bus-verb"
  (letc ([:size 0.4]
         [:dampen 0.8]
         [:inbus :verb1])
    (let* ([l (in 1 ar (private-bus :inbus))]
           [r (in 1 ar (add 1 (private-bus :inbus)))]
           [sig (free-verb2 l r 1 :size :dampen)])
      (out 0 sig))))

(send-synth sc3 "bus-delay"
  (letc ([:time-l 0.5]  [:time-r 1.0]
         [:decay-l 2.0] [:decay-r 1.5]
         [:width 0.5]   
         [:tempo 0.5]
         [:inbus :delay1])
    (let* ([t (fdiv 0.25 :tempo)]
           [l (in 1 ar (private-bus :inbus))]
           [r (in 1 ar (add 1 (private-bus :inbus)))]
           [l (comb-c l 4.0 (mul :time-l t) :decay-l)]
           [r (comb-c r 4.0 (mul :time-r t) :decay-r)])
      (out 0 (add (make-pan l (add 0.5 (mul -0.5 :width)))
                  (make-pan r (add 0.5 (mul  0.5 :width))))))))

(start-bus-effect "bus-verb"  (pair ":inbus" :verb1))
(start-bus-effect "bus-delay" (pair ":inbus" :delay1))