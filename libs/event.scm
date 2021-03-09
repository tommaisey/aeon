;; -*- geiser-scheme-implementation: chez-*-
;;-------------------------------------------------------------
;; Data structure and associated functions representing an event.
;;
;; An event is really just an associative dictionary,
;; mapping keys to values. The most important key is 'beat'
;; which describes its position in time.
;; It's implemented as an immutable alist - every operation
;; returns a new list, the old one remains untouched.
;; ------------------------------------------------------------
(library (event)
  (export :beat :sustain
          time-key
          priority-keys
          make-event
          make-event-fast
          event-get
          event-set
          event-update
          event-check
          event-clean
          event-optimise
          event-prioritise
          event-symbols->strings
          event-set-multi
          event-get-multi
          event-remove-multi

          event-beat
          event-before?
          event-move
          print-event
          print-events
          make-events-with-times)

  (import (chezscheme) (utilities)
          (only (srfi s1 lists) delete-duplicates lset-difference))

  (define :beat ':beat)
  (define :sustain ':sustain)
  (define time-key :beat)
  (define priority-keys '(:sustain :freq :beat))

  ;; Returns an event: (make-event 1/16 :freq 440 :amp 0.1)
  (define (make-event start-beat . key-values)
    (cons (cons :beat start-beat) (apply make-alist key-values)))

  ;; For internal use: less convenient syntax, but faster
  (define-syntax make-event-fast
    (syntax-rules ()
      ((_ start-beat (key value) ...)
       (list (cons :beat start-beat) (cons key value) ...))))

  ;; Getters & setters
  (define (event-get event key default)
    (alist-get event key default))

  (define (event-set event key value)
    (unless (symbol? key)
      (error 'event-set "trying to set a non-symbol event key" key))
    (alist-set event key value))
  
  (define (event-update event key update-fn default)
    (event-set event key (update-fn (event-get event key default))))

  ;; Convenience fns for setting/getting multiple values
  (define-syntax event-set-multi
    (syntax-rules ()
      ((_ event (key value) rest ...)
       (cons (cons key value) (event-set-multi event rest ...)))

      ((_ event) event)))

  (define (event-get-multi event key-default-pairs)
    (alist-get-multi event key-default-pairs))

  (define (event-remove-multi event key-list)
    (lset-difference (lambda (entry k) (eq? k (car entry))) event key-list))

  ;; Time-related helpers
  (define (event-beat event)
    (lest [result (assq time-key event)]
          (cdr result)
          (error 'event-beat "no :beat key in event!" event)))
  (define (event-move e n math-fn)
    (event-update e time-key (lambda (t) (math-fn t n)) 0))
  (define (event-before? e1 e2)
    (< (event-beat e1) (event-beat e2)))
  
  ;; Checks there is an item at the key and that it
  ;; satisfies the predicate (which may have extra args)
  (define (event-check event key pred . args)
    (let ([v (event-get event key #f)])
      (if v (apply pred (cons v args)) #f)))

  ;; Copy commonly used entries to the front of the alist,
  ;; accelerating future searches for them.
  (define (event-prioritise event key)
    (let ([result (event-get event key #f)])
      (if result (event-set event key result) event)))

  ;; Remove the 'history' of the alist. One entry per key.
  (define (event-clean event)
    (delete-duplicates event (lambda (x y) (eq? (car x) (car y)))))

  ;; Prioritise and clean the event (see above).
  (define (event-optimise event)
    (let ([n (fold-left event-prioritise event priority-keys)])
      (event-clean n)))

  ;; Turn all the symbol keys into string keys
  (define (event-symbols->strings e)
    (define (pair-symbol->string kv-pair)
      (cons (symbol->string (car kv-pair)) (cdr kv-pair)))
    (map pair-symbol->string e))

  ;; Some event convenience functions
  (define (print-event event port)
    (define (format-val v)
      (format "~[~,2F~:;~S~]" (if (and (number? v) (inexact? v)) 0 1) v))
              (display "[" port)
              (for-each
                (lambda (kv)
                  (display (car kv) port)
                  (display ": " port)
                  (display (format-val (cdr kv)) port)
                  (display " " port))
                (event-optimise event))
              (display "]" port)
              (newline port))

  (define (print-events event-list port)
    (for-each (lambda (e) (print-event e port)) event-list))

  (define (make-events-with-times times-list)
    (map (lambda (t) (make-event t)) times-list))

  ) ; END module 'event'
