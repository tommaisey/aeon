;; This file will serve as a nursery for SuperCollider SynthDefs and related
;; things like sample management. It's not a library, so can be loaded in
;; its entirety to update definitions live.

;; Useful test function: (test-synth "sine-grain" :freq 330 :attack 1/16)
(define (test-synth name . args)
  (play-now name (event-symbols->strings (apply make-alist args))))

;;------------------------------------------------------------------
;; Keyword for specifying which synthdef an event applies to.
(declare-keywords :inst)

;; Synths using tempo-based effects can have this property.
;; Its value should be set to the number of seconds per beat.
;; See also tempo-dependent-keys, which are preprocessed by the
;; client to be relative to the tempo.
(declare-keywords :tempo)

;; Pre-declare these so that we can use them below:
(declare-keywords :attack :sustain :release)

;; Abbreviated aliases for common envelope keywords.
(alias :atk :attack)
(alias :sus :sustain)
(alias :rel :release)

;; Time-based properties that will be preprocessed to be relative to the tempo.
;; They are paired with #f so this list can be used directly with alist-get-multi.
(define tempo-dependent-keys
  (make-alist :sustain #f :attack #f :release #f))

;;-------------------------------------------------------------------
;; The synthdef nursery...
(send-synth sc3 "sine-grain"
  (src-synth ([:freq 440 kr 0.05] [:attack 0.01] [:sustain 1])
    (* (sin-osc ar :freq 0)
       (make-ar :attack :sustain))))

(send-synth sc3 "fm-grain"
  (src-synth ([:freq 440 kr 0.05] [:attack 0.01] [:sustain 1]
              [:ratio 2] [:fm-amt 0.1])
    (let* ([mod (sin-osc ar (* :ratio :freq) 0.5)]
           [fm-env (make-ar :attack (* :sustain 1.5) :fm-amt -6)])
      (* (sin-osc ar :freq (* mod fm-env))
         (make-ar :attack :sustain)))))

(send-synth sc3 "sampler"
  (src-synth ([:attack 0.01] [:sustain 1] [:release 0.25]
              [:sample 0] [:speed 1]
              [:resonance 0.0] [:cutoff 1.0]
              [:sample-pos 0.0])

    (let* ([rate (* :speed (buf-rate-scale kr :sample))]
           [frames (buf-frames kr :sample)]
           [pos (* frames :sample-pos)]
           [osc (play-buf 1 ar :sample rate 1 pos no-loop remove-synth)])
      (* (moog-ff osc (scale-cutoff :cutoff) :resonance 0)
         (make-asr :attack :sustain :release)))))

(send-synth sc3 "swirly-keys"
  (src-synth ([:attack 0.4] [:sustain 1] [:release 1]
              [:freq 440 kr 0.1] [:cutoff 3] [:resonance 0.1])

     (let* ([:amp (* :amp (lin-exp :freq 20 10000 1.0 0.2))]
            [osc1 (lf-saw ar (* :freq 0.4978) 0)]
            [osc2 (lf-saw ar (* :freq 1.0024) 0.3)]
            [cut-env (make-asr :attack :sustain :release :cutoff 0.5 do-nothing)]
            [cutoff (clamp-cutoff (+ :freq (* :freq cut-env)))])
       (* (moog-ff (+ osc1 osc2) cutoff :resonance 0.0)
          (make-adsr :attack :attack :sustain :release 0.4)))))

(send-synth sc3 "phase-tris"
  (src-synth ([:attack 0.4] [:sustain 1] [:release 1]
              [:freq 440] [:slop 0.05] [:cutoff 0.8])

    (define (make-osc freq-mul time mag)
      (let ([f (* :freq freq-mul)]
            [s (* :slop mag)])
        (lf-tri ar (+ f (make-rand-lfo (* f s) time)) 0)))

    (let* ([osc1 (make-osc 0.5 1/2 0.05)]
           [osc2 (make-osc 1.0 4/3 0.04)]
           [osc3 (make-osc 1.5 3/2 0.01)]
           [sig (+ osc1 osc2 osc3)])
      (* (rlpf sig (scale-cutoff :cutoff) 1.0) 
         (make-asr :attack :sustain :release)))))

(send-synth sc3 "dual-lopass"
  (src-synth ([:freq 440 kr 0.175]
              [:attack 2.0] [:sustain 1] [:release 1]
              [:cutoff1 1.0 kr 0.5] [:cutoff2 2.0 kr 0.5] [:resonance 0.1])

    (define (make-osc freq flt-atk cutoff-prop)
      (let* ([pulse-lfo (sin-osc kr 0.5 0)]
             [osc (lf-pulse ar freq 0  (*+ pulse-lfo 0.35 0.5))]
             [sub (lf-pulse ar (* 0.5 freq) 0.4  (*+ pulse-lfo -0.15 0.5))]
             [osc-flt-env (make-asr flt-atk :sustain :release 1 (list 4 -4) do-nothing)]
             [osc-flt-env (+ (* 4 freq) (* 4 freq cutoff-prop osc-flt-env))]
             [osc-flt-env (clamp-cutoff osc-flt-env)])
        (moog-ff (+ osc (* sub 0.8)) osc-flt-env :resonance 0)))

    (let* ([:amp (* :amp (lin-exp :freq 20 10000 1.0 0.2))]
           [osc1 (make-osc (* :freq 0.5) (* :attack 2) :cutoff1)]
           [osc2 (make-osc :freq (* :attack 1.5) :cutoff2)])
      (* (+ osc1 osc2)
         (make-asr :attack :sustain :release 1 (list 4 -4))))))

(send-synth sc3 "pulse-pluck"
  (src-synth ([:freq 440 kr 0.05]
              [:attack 0.01] [:sustain 1]
              [:cutoff 0.5] [:resonance 0])

    (let* ([line (make-line (rand 0.1 0.9) (rand 0.1 0.9) :sustain)]
           [osc (lf-pulse ar :freq 0 line)]
           [cutoff (scale-cutoff :cutoff)])
      (* (moog-ff osc cutoff :resonance 0)
         (make-ar :attack :sustain)))))

;;-------------------------------------------------------------------
;; The fx synthdef nursery...
(send-synth sc3 "hpf"
  (fx-synth ([:cutoff 0.5] [:resonance 0] [:sustain 1])
    (* (rhpf :in (scale-cutoff :cutoff) :resonance)
       (make-ar 0.005 :sustain))))

;;-----------------------------------------------------------------------
;; Bus effects
(send-synth sc3 "bus-verb"
  (letc ([:size 0.4]
         [:dampen 0.8]
         [:inbus :verb1])
    (let* ([l (in 1 ar (private-bus :inbus))]
           [r (in 1 ar (+ 1 (private-bus :inbus)))])
      (out 0 (free-verb2 l r 1 :size :dampen)))))

(send-synth sc3 "bus-delay"
  (letc ([:time-l 0.5]  [:time-r 1.0]
         [:decay-l 2.0] [:decay-r 1.5]
         [:width 0.5]   
         [:tempo 0.5]
         [:inbus :delay1])
    (let* ([t (fdiv 0.25 :tempo)]
           [l (in 1 ar (private-bus :inbus))]
           [r (in 1 ar (+ 1 (private-bus :inbus)))]
           [l (comb-c l 4.0 (* :time-l t) :decay-l)]
           [r (comb-c r 4.0 (* :time-r t) :decay-r)])
      (out 0 (+ (make-pan l (*+ :width -0.5 0.5))
                (make-pan r (*+ :width  0.5 0.5)))))))

(start-send-effect "bus-verb"  (pair ":inbus" :verb1))
(start-send-effect "bus-delay" (pair ":inbus" :delay1))
