;; This file will serve as a nursery for SuperCollider SynthDefs and related
;; things like sample management. It's not a library, so can be loaded in
;; its entirety to update definitions live.

;; A useful test function
(define* (test-synth name [/opt (freq 440) (length 1)])
  (play-now name
            (cons ":freq" freq)
            (cons ":sustain" length)))

(define* (sine-perc [/opt (freq 440) (length 1)])
  (test-synth "sine-grain" freq length))

;;------------------------------------------------------------------
;; Keyword for specifying which synthdef an event applies to.
(declare-keywords :inst)

;; Synths using tempo-based effects can have this property.
;; Its value should be set to the number of seconds per beat.
;; See also tempo-dependent-keys, which are preprocessed by the
;; client to be relative to the tempo.
(declare-keywords :tempo)

;; Events with :group will be added to the node group with the value
;; as its ID. Events with :control must also have :group - control properties
;; will be set on all synths in that node group instead of creating new synths.
(declare-keywords :group :control)

;; If the user supplies a vector of samples, for the :sample key, this key can
;; be used to select between them.
(declare-keywords :sample-idx)
(alias :sidx :sample-idx)

;; Defines some globally reserved bus numbers. These are used for
;; standard effects busses in the synthdefs. The numbers start at
;; 16. Buses are all stereo.
(define (get-bus-num i) (* i 2))
(define :verb1  (get-bus-num 0))
(define :verb2  (get-bus-num 1))
(define :delay1 (get-bus-num 2))

;;-------------------------------------------------------------------
;; Some helper tools for writing synthdefs more clearly.
;; N-arity versions of rsc3 mul, sub & add.
(define-syntax *u
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (sc/mul x (*u y ...)))))

(define-syntax +u
  (syntax-rules ()
    ((_ x) x)
    ((_ x y ...) (sc/add x (+u y ...)))))

(define-syntax -u
  (syntax-rules ()
    ((_ x) (*u x -1))
    ((_ x y ...) (sc/sub x (+u y ...)))))

(alias *+u sc/mul-add)

(define (private-bus i)
  (+u i sc/num-output-buses sc/num-input-buses))

(define (make-env-gen magnitude env action)
  (sc/env-gen sc/kr 1 magnitude 0 1 action env))

(define* (make-asr magnitude atk sus rel [/opt (curve -4) (action sc/remove-synth)])
  (let ([shape (sc/env-linen atk sus rel 1 (repeat 4 curve))])
    (make-env-gen magnitude shape action)))

(define (make-rand-lfo mag time)
  (*u (sc/lfd-noise1 sc/ar time) mag))

(define (make-pan sig pos)
  (sc/pan2 sig (*+u pos 2 -1) 1))

(define (make-bus-out bus sig pan)
  (sc/out bus (make-pan sig pan)))


;; Makes N parallel outputs, where N is the number of pairs.
;; each pair should be: (bus-num . send-amt)
(define (make-outputs sig pan . pairs)
  (if (null? pairs)
      (raise "Must supply bus info pairs to make-outputs")
      (let* ([panned (make-pan sig pan)]
             [make-out (lambda (info) (sc/out (car info) (*u panned (cdr info))))]
             [combine (lambda (ug info) (sc/mrg2 ug (make-out info)))])
        (fold-left combine (make-out (car pairs)) (cdr pairs)))))

;; Synthdefs containing filters should be careful to keep
;; cutoff frequencies in a safe range.
(define (clamp-cutoff cutoff)
  (sc/clip cutoff 30 19000))

;; Override this rsc3 macro (which defines synthdef args) to also make
;; a top level 'keyword' (i.e. self-evaluating symbol) out of the name.
(define-syntax letc
  (syntax-rules ()
    ((_ () expr rest ...)
     (begin expr rest ...))

    ((_ ((name default) ...) expr rest ...)
     (let ((name (begin
                   (define-top-level-value 'name 'name)
                   (sc/make-control* (symbol->string 'name) default sc/kr 0)))
           ...)
       expr rest ...))))

;;-------------------------------------------------------------------
;; The synthdef nursery - quite bare at the moment!
(sc/send-synth sc3 "sine-grain"
  (letc ([:freq 440] [:attack 0.01] [:sustain 1]
         [:amp 0.5] [:pan 0.5]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (let* ([osc (sc/sin-osc sc/ar (sc/lag :freq 0.05) 0)]
           [env (sc/env-perc :attack :sustain 1 (repeat 2 -4))]
           [env (make-env-gen 1 env sc/remove-synth)]
           [sig (*u env osc)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "fm-grain"
  (letc ([:freq 440] [:attack 0.01] [:sustain 1]
         [:amp 0.5] [:pan 0.5]
         [:ratio 2] [:fm-amt 0.1]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (let* ([frq (sc/lag :freq 0.05)]
           [mod (sc/sin-osc sc/ar (*u :ratio frq) 0.5)]
           [fm-env (sc/env-perc 0.0 (*u :sustain 1.5) :fm-amt (repeat 2 -6))]
           [fm-env (make-env-gen 1 fm-env sc/remove-synth)]
           [osc (sc/sin-osc sc/ar frq (*u mod fm-env))]
           [env (sc/env-perc :attack :sustain 1 (repeat 2 -4))]
           [env (make-env-gen 1 env sc/remove-synth)]
           [sig (*u env osc)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "sampler"
  (letc ([:attack 0.01] [:sustain 1] [:release 0.25]
         [:sample 0] [:speed 1]
         [:amp 0.3] [:pan 0.5]
         [:resonance 0.0] [:cutoff 1.0]
         [:sample-pos 0.0]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (let* ([rate (*u :speed (sc/buf-rate-scale sc/kr :sample))]
           [frames (sc/buf-frames sc/kr :sample)]
           [pos (*u frames :sample-pos)]
           [osc (sc/play-buf 1 sc/ar :sample rate 1 pos sc/no-loop sc/remove-synth)]
           [osc (sc/moog-ff osc (*+u :cutoff 17000 75) :resonance 0)]
           [env (make-asr 1 :attack :sustain :release -4)]
           [sig (*u env osc)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "swirly-keys"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
         [:freq 440] [:amp 0.2] [:pan 0.5]
         [:cutoff 3] [:resonance 0.1]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (let* ([:freq (sc/lag :freq 0.1)]
           [:pan  (sc/lag :pan 0.15)]
           [:amp (*u :amp (sc/lin-exp :freq 20 10000 1.0 0.2))]
           [osc1 (sc/lf-saw sc/ar (*u :freq 0.9946) 0)]
           [osc2 (sc/lf-saw sc/ar (*u :freq 1.0024) 0.3)]
           [amp-env (make-asr 1 :attack :sustain :release -4)]
           [cut-env (make-asr :cutoff :attack :sustain :release 2 sc/do-nothing)]
           [cutoff (clamp-cutoff (+u :freq (*u :freq cut-env)))]
           [flt (sc/moog-ff (+u osc1 osc2) cutoff :resonance 0.0)]
           [sig (*u amp-env flt)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "phase-tris"
  (letc ([:attack 0.4] [:sustain 1] [:release 1]
         [:freq 440] [:amp 0.2] [:pan 0.5]
         [:slop 0.05] [:cutoff 0.9]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (define (make-osc mag freq-mul time)
      (let ([freq (*u :freq freq-mul)])
        (sc/lf-tri sc/ar (+u freq (make-rand-lfo (*u freq mag) time)) 0)))

    (let* ([osc1 (make-osc (*u :slop 0.05) 0.5 1/2)]
           [osc2 (make-osc (*u :slop 0.04) 1.0 4/3)]
           [osc3 (make-osc (*u :slop 0.01) 1.5 3/2)]
           [env (make-asr 1 :attack :sustain :release -4)]
           [sig (*u env (+u osc1 osc2 osc3))]
           [cutoff (*+u (+u -1 :cutoff) 17000 200)]
           [sig (sc/rlpf sig (clamp-cutoff cutoff) 1.0)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "dual-lopass"
  (letc ([:attack 2.0] [:sustain 1] [:release 1]
         [:freq 440] [:amp 0.2] [:pan 0.5]
         [:resonance 0.1] [:cutoff1 1.0] [:cutoff2 2.0]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (define (make-osc freq flt-atk cutoff-prop)
      (let* ([pulse-lfo (sc/sin-osc sc/kr 0.5 0)]
             [osc (sc/lf-pulse sc/ar freq 0 (+u 0.50 (*u 0.35 pulse-lfo)))]
             [sub (sc/lf-pulse sc/ar (*u 0.5 freq) 0.4 (+u 0.50 (*u 0.15 pulse-lfo)))]
             [osc-flt-env (make-asr 1 flt-atk :sustain :release 1 sc/do-nothing)]
             [osc-flt-env (+u (*u 4 freq) (*u 4 freq cutoff-prop osc-flt-env))]
             [osc-flt-env (clamp-cutoff osc-flt-env)])
        (sc/moog-ff (+u osc (*u sub 0.8)) osc-flt-env :resonance 0)))

    (let* ([:freq (sc/lag :freq 0.175)]
           [:pan (sc/lag :pan 0.15)]
           [:cutoff1 (sc/lag :cutoff1 0.5)]
           [:cutoff2 (sc/lag :cutoff2 0.5)]
           [:amp (*u :amp (sc/lin-exp :freq 20 10000 1.0 0.2))]
           [osc1 (make-osc (*u :freq 0.5) (*u :attack 2) :cutoff1)]
           [osc2 (make-osc :freq (*u :attack 1.5) :cutoff2)]
           [amp-env (make-asr 1 :attack :sustain :release -4)]
           [sig (*u (+u osc1 osc2) amp-env)])
      (make-outputs sig :pan
                    (pair 0 :amp)
                    (pair (private-bus :bus1) :bus1-amt)
                    (pair (private-bus :bus2) :bus2-amt)))))

(sc/send-synth sc3 "pulse-pluck"
  (letc ([:freq 440] [:attack 0.01] [:sustain 1]
         [:amp 0.5] [:pan 0.5] [:cutoff 0.5] [:resonance 0]
         [:bus1 :verb1]  [:bus1-amt 0]
         [:bus2 :delay1] [:bus2-amt 0])

    (let* ([line (sc/line sc/kr (sc/rand 0.1 0.8) (sc/rand 0.1 0.8) :sustain sc/do-nothing)]
           [osc (sc/lf-pulse sc/ar (sc/lag :freq 0.05) 0 line)]
           [env (sc/env-perc :attack :sustain 1 (repeat 2 -4))]
           [env (make-env-gen 1 env sc/remove-synth)]
           [sig (*u env osc)]
           [cutoff (+u 75 (*u :cutoff 17000))]
           [sig (sc/moog-ff sig cutoff :resonance 0)])
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
(sc/send-synth sc3 "bus-verb"
  (letc ([:size 0.4]
         [:dampen 0.8]
         [:inbus :verb1])
    (let* ([l (sc/in 1 sc/ar (private-bus :inbus))]
           [r (sc/in 1 sc/ar (+u 1 (private-bus :inbus)))]
           [sig (sc/free-verb2 l r 1 :size :dampen)])
      (sc/out 0 sig))))

(sc/send-synth sc3 "bus-delay"
  (letc ([:time-l 0.5]  [:time-r 1.0]
         [:decay-l 2.0] [:decay-r 1.5]
         [:width 0.5]   
         [:tempo 0.5]
         [:inbus :delay1])
    (let* ([t (sc/fdiv 0.25 :tempo)]
           [l (sc/in 1 sc/ar (private-bus :inbus))]
           [r (sc/in 1 sc/ar (+u 1 (private-bus :inbus)))]
           [l (sc/comb-c l 4.0 (*u :time-l t) :decay-l)]
           [r (sc/comb-c r 4.0 (*u :time-r t) :decay-r)])
      (sc/out 0 (+u (make-pan l (+u 0.5 (*u -0.5 :width)))
                  (make-pan r (+u 0.5 (*u  0.5 :width))))))))

(start-bus-effect "bus-verb"  (pair ":inbus" :verb1))
(start-bus-effect "bus-delay" (pair ":inbus" :delay1))