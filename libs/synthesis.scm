(library (synthesis)

  (export send-synth 
          letc src-synth
          get-bus-num
          private-bus
          make-env-gen
          make-asr
          make-adsr
          make-ar
          make-line
          make-rand-lfo
          make-pan
          make-bus-out
          make-outputs
          scale-cutoff
          clamp-cutoff
          :verb1 :verb2 :delay1)

  (import (scheme)
          (utilities)
          (prefix (rsc3) sc/)
          (prefix (sosc) so/))

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
      ((or (number? c) (symbol? c)) 
       (repeat num-segments c))
      ((unsafe-list? c)
       (if (eq? (length c) num-segments) c 
           (extend-repeating-last c num-segments)))
      (else (error 'curve "curve argument should be a number, symbol or list" c))))

  ;;-------------------------------------------------------------------
  (define (make-rand-lfo mag time)
    (sc/mul (sc/lfd-noise1 sc/ar time) mag))

  (define (make-pan sig pos)
    (sc/pan2 sig (sc/mul-add pos 2 -1) 1))

  (define (make-bus-out bus sig pan)
    (sc/out bus (make-pan sig pan)))

  ;; Makes N parallel outputs, where N is the number of pairs.
  ;; each pair should be: (bus-num . send-amt)
  (define (make-outputs sig pan . pairs)
    (if (null? pairs)
        (raise "Must supply bus info pairs to make-outputs")
        (let* ([panned (make-pan sig pan)]
               [make-out (lambda (info) (sc/out (car info) (u* panned (cdr info))))]
               [combine (lambda (ug info) (sc/mrg2 ug (make-out info)))])
          (fold-left combine (make-out (car pairs)) (cdr pairs)))))

  ;; Synthdefs containing filters should be careful to keep
  ;; cutoff frequencies in a safe range.
  (define* (clamp-cutoff x [/opt (min 30) (max 19000)])
    (sc/clip x min max))

  (define* (scale-cutoff f [/opt (min 30) (max 17000)])
    (clamp-cutoff (sc/mul-add f max min) min max))

  ;;-------------------------------------------------------------------
  ;; Variadic versions of the common binary ops.
  (define (fold-binary-op op args)
    (fold-left (lambda (x y) (op x y)) (car args) (cdr args)))
  (define (u* . args)
    (fold-binary-op sc/mul args))
  (define (u+ . args)
    (fold-binary-op sc/add args))
  (define (u- . args)
    (fold-binary-op sc/sub args))

  ;; Inside send-synth, variadic versions of rsc3's mul, sub & add
  ;; ugens are aliased over the normal arithmetic operators.
  ;; We also import (rsc3) without a prefix.
  (define-syntax send-synth
    (lambda (x)
      (syntax-case x ()
        ((k sc3 name-str body ...)
         (with-identifiers #'k (* + - *+ rsc3 letc)
           #'(sc/send-synth sc3 name-str
               (let ([* u*] [+ u+] [- u-] [*+ sc/mul-add])
                 (import (except (rsc3) letc))
                 body ...)))))))

  ;; Makes and binds synthedef inputs (i.e. Control objects) and also defines
  ;; the names of said inputs as 'keywords', i.e. self-evaluating symbols.
  (define-syntax letc
    (syntax-rules  ()
      ((_ ([name default] ...) extra-defs ... signal)
       (let ([name (begin (define-top-level-value 'name 'name)
                          (sc/make-control* (symbol->string 'name) default sc/kr 0))]
             ...)
         extra-defs ...
         signal))))

  ;; Define a standard in/out structure for a sound source like a synth
  ;; or sampler voice. Predefines :out, :amp, :pan and some sends.
  (define-syntax src-synth
    (lambda (x)
      (syntax-case x ()
        ((k ([name default] ...) extra-defs ... signal)
         (with-identifiers #'k [:out :amp :pan :send1 :send2 :dest1 :dest2]
           #'(letc ([:out 0] ; TODO: ir rate 
                    [:amp 0.5] [:pan 0.5]
                    [:send1 0] [:send2 0]
                    [:dest1 :verb1] [:dest2 :delay1]
                    [name default] ...)
               extra-defs ...
               (make-outputs signal :pan
                 (pair :out :amp)
                 (pair (private-bus :dest1) :send1)
                 (pair (private-bus :dest2) :send2))))))))
  
  )