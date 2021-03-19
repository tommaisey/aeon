;; Convenience functions and macros that streamline our use of
;; the rsc3 library to communicate with SuperCollider.
(library (synthesis)

  (export send-synth 
          letc src-synth fx-synth
          get-bus-num
          private-bus
          make-env-gen
          make-asr
          make-adsr
          make-ar
          make-line
          make-rand-lfo
          make-pan
          stereo-in
          scale-cutoff
          clamp-cutoff
          convert-q
          freq->amp
          :group :fx :control 
          :verb1 :verb2 :delay1)

  (import (scheme)
          (utilities)
          (prefix (rsc3) sc/))

  ;; Events with :group will be added to the given node group.
  ;; Events with :fx will be added to the end of the given node group
  ;; Events with :control will send control messages to all nodes in
  ;; the :group they are assigned to.
  (declare-keywords :group :control :fx)

  ;; Defines some globally reserved bus numbers. These are used for
  ;; standard effects busses in the synthdefs. The numbers start at
  ;; 16. Buses are all stereo.
  (define (get-bus-num i) (* i 2))
  (define :verb1  (get-bus-num 0))
  (define :verb2  (get-bus-num 1))
  (define :delay1 (get-bus-num 2))

  (define (private-bus i)
    (u+ i sc/num-output-buses sc/num-input-buses))

  ;;-------------------------------------------------------------------
  ;; Envelopes generator - plays back envelope stages.
  (define rm-synth sc/remove-synth)
  
  (define* (make-env-gen env [/opt (peak 1) (action rm-synth)])
    (sc/env-gen sc/kr 1 peak 0 1 action env))

  ;; Attack Sustain Release
  (define* (make-asr atk sus rel 
                     [/opt (peak 1) (curve -4) (action rm-synth)])
    (let ([env (sc/env-linen atk sus rel 1 (convert-curve curve 4))])
      (make-env-gen env peak action)))

  ;; Attack Decay Sustain Release
  (define* (make-adsr atk dec sus rel susLvl 
                      [/opt (peak 1) (curve -4) (action rm-synth)])
    (let ([env (sc/env (list 0.0 1 susLvl susLvl 0.0)
                       (list atk dec sus rel)
                       (convert-curve curve 4)
                       -1 -1)])
      (make-env-gen env peak action)))

  ;; Attack Release
  (define* (make-ar atk rel [/opt (peak 1) (curve -4) (action rm-synth)])
    (let ([env (sc/env-perc atk rel 1 (convert-curve curve 2))])
      (make-env-gen env peak action)))

  (define (make-line start end len)
    (sc/line sc/kr start end len sc/do-nothing))

  (define (convert-curve c num-segments)
    (cond
      [(or (number? c) (symbol? c))
       (repeat num-segments c)]
      [(unsafe-list? c)
       (if (eq? (length c) num-segments) c
           (extend-repeating-last c num-segments))]
      [else (error 'curve curve-err c)]))

  (define curve-err
    "curve argument should be a number, symbol or list")

  ;;-------------------------------------------------------------------
  (define (make-rand-lfo mag time)
    (sc/mul (sc/lfd-noise1 sc/ar time) mag))

  (define (make-pan pos sig)
    (sc/pan2 sig (sc/mul-add pos 2 -1) 1))

  ;; 'in' can read multiple channels but can only output one, so
  ;; here we multichannel expand two of them.
  (define (stereo-in inbus)
    (sc/mce2 (sc/in 1 sc/ar inbus) (sc/in 1 sc/ar (u+ inbus 1))))

  ;; Synthdefs containing filters should be careful to keep
  ;; cutoff frequencies in a safe range.
  (define* (clamp-cutoff f [/opt (min 30) (max 18000)])
    (sc/clip f min (sc/u:min max (u* sc/sample-rate 0.45))))

  (define* (scale-cutoff f [/opt (min 30) (max 18000)])
    (clamp-cutoff (sc/lin-exp f 0 1 min max) min max))

  (define (convert-q resonance)
    (sc/clip (sc/lin-lin resonance 0 1 1 0.01) 0 1))

  ;; Makes higher-pitched sounds quieter. Adds a little extra
  ;; gain, because most notes will be attenuated by amp-comp.
  ;; These values were determined entirely unscientifically.
  (define* (freq->amp freq amp [/opt (base-hz 50) (exp 0.11)])
    (let ([f (sc/u:max freq base-hz)]
          [r (sc/rate-of freq)])
      (u* 1.25 amp (sc/amp-comp r f base-hz exp))))

  ;;-------------------------------------------------------------------
  ;; Variadic versions of the common binary ops.
  (define (fold-binary-op arith-op sc-op args)
    (if (for-all number? args)
        (apply arith-op args)
        (fold-left sc-op (car args) (cdr args))))
  
  (define (u* . args)
    (fold-binary-op * sc/mul args))
  (define (u- . args)
    (fold-binary-op - sc/sub args))
  (define (u/ . args)
    (fold-binary-op / sc/fdiv args))
  (define (u+ . args) ;; TODO: optimise to sum3 or sum4
    (fold-binary-op + sc/add args))

  ;; Inside send-synth, variadic versions of rsc3's mul, sub & add
  ;; ugens are aliased over the normal arithmetic operators.
  ;; We also import (rsc3) without a prefix.
  (define-syntax send-synth
    (lambda (x)
      (syntax-case x ()
        ((k sc3 name-str body ...)
         (with-identifiers #'k (* + - / *+ rsc3 letc)
           #'(sc/send-synth sc3 name-str
               (let ([* u*] [+ u+] [- u-] [/ u/] [*+ sc/mul-add])
                 (import (except (rsc3) letc))
                 body ...)))))))

  ;; Makes and binds synthdef inputs (i.e. Control objects) and also defines
  ;; the names of said inputs as 'keywords', i.e. self-evaluating symbols.
  (define-syntax letc
    (syntax-rules ()
      ((_ ([name default rest ...] ...) extra-defs ... signal)
       (let* ([name (begin (define-top-level-value 'name 'name)
                           (make-control 'name default rest ...))]
             ...)
         extra-defs ...
         signal))))

  (define* (make-control name default [/opt (rate sc/kr) (lag 0)])
    (sc/make-control* (symbol->string name) default rate lag))

  ;; Define a standard in/out structure for a sound source like a synth
  ;; or sampler voice. Predefines :out, :amp, and :pan.
  (define-syntax src-synth
    (lambda (x)
      (syntax-case x ()
        ((k ([name default rest ...] ...) extra-defs ... signal)
         (with-identifiers #'k [:out :amp :pan]
           #'(letc ([:out 0 sc/ir]
                    [:amp 0.5 sc/kr 0.05]
                    [:pan 0.5 sc/kr 0.05]
                    [name default rest ...] ...)
               extra-defs ...
               (sc/out :out (make-pan :pan signal))))))))

  ;; Define an effect synth. Takes input from a bus and
  ;; replaces its contents with the effected version.
  ;; Predefines :in as an In.ar ugen pointed at the right bus.
  (define-syntax fx-synth
    (lambda (x)
      (syntax-case x ()
        ((k ([name default rest ...] ...) extra-defs ... signal)
         (with-identifiers #'k [:in :sustain :fade]
           #'(letc ([:in 1023 sc/ir]
                    [:sustain 1 sc/ir]
                    [:fade 0.01 sc/ir]
                    [name default rest ...] ...)
               (let* ([:inbus :in]
                      [:in (sc/in 2 sc/ar :inbus)]
                      [env (make-asr :fade :sustain :fade)]
                      [env (sc/mul-add env 2 -1)]
                      [xfade sc/x-fade2])
                 extra-defs ...
                 (sc/replace-out :inbus (xfade :in signal env 1)))))))))

  )
