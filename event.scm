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
  (export :beat
	  time-key
	  priority-keys
	  make-event
	  event-get
	  event-set
	  event-update
	  event-check
	  event-clean
	  event-optimise
	  event-prioritise
	  event-get-multi
	  event-remove-multi
	  
	  event-beat
	  event-before?
	  event-move
	  print-event
	  print-events
	  make-events-with-times
	  make-events-regular
   
	  arc arc?
	  make-arc
	  arc-with-start
	  arc-with-end
	  arc-start
	  arc-end
	  arc-length
	  arc-negate
	  arc-add
	  arc-valid?
	  within-arc?
	  arcs-overlap?)

  (import (chezscheme) (utilities) (srfi s26 cut)
	  (only (srfi s1 lists) delete-duplicates lset-difference))

  (define :beat ':beat)
  (define time-key :beat)
  (define priority-keys '(:sustain :freq :beat))

  (define-syntax make-event
    (syntax-rules ()
      ((_ start-beat (key value) ...)
       (list (cons :beat start-beat) (cons key value) ...))
      
      ((_ ...)
       (syntax-error "make-event syntax: (make-event 1/16 [:freq 100] [:pan 0]"))))

  (define (event-get event key default)
    (let ([result (assq key event)])
      (if result (cdr result) default)))
  (define (event-set event key value)
    (cons (cons key value) event))
  (define (event-update event key update-fn default)
    (event-set event key (update-fn (event-get event key default))))
  (define (event-beat event)
    (event-get event time-key 0))
  (define (event-move e n math-fn)
    (event-update e time-key (lambda (beat) (math-fn beat n)) 0))
  (define (event-before? n1 n2)
    (< (event-beat n1) (event-beat n2)))
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
    (let ([n (fold-left (cut event-prioritise <> <>) event priority-keys)])
      (event-clean n)))

  (define (event-get-multi event key-default-pairs)
    (alist-get-multi event key-default-pairs))
  
  (define (event-remove-multi event key-list)
    (lset-difference (lambda (entry k) (eq? k (car entry))) event key-list))

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

  (define make-events-regular
    (case-lambda
      ((num interval start)
       (define (impl lst num t)
	 (if (<= num 0) lst
	     (impl (cons t lst) (sub1 num) (+ t interval))))
       (make-events-with-times (reverse (impl '() num start))))
      
      ((num interval) ; start is optional
       (make-events-regular num interval 0))))

  ;;--------------------------------------------------
  ;; A range of time (in beats)
  (define-record-type arc
    (fields (immutable start)
	    (immutable end)))

  (define (arc-with-start a new-start)
    (make-arc new-start (arc-end a)))
  (define (arc-with-end a new-end)
    (make-arc (arc-start a) new-end))
  (define (arc-valid? a)
    (< (arc-start a) (arc-end a)))
  (define (within-arc? a t)
    (between-inclusive t (arc-start a) (arc-end a)))
  (define (arcs-overlap? a b)
    (or (within-arc? b (arc-start a))
	(within-arc? b (arc-end a))
	(within-arc? a (arc-start b))
	(within-arc? a (arc-end b))))
  (define (arc-length a)
    (- (arc-end a) (arc-start a)))
  (define (arc-negate a)
    (make-arc (- (arc-start a)) (- (arc-end a))))
  (define (arc-add a1 a2)
    (make-arc (+ (arc-start a1) (arc-start a2))
	      (+ (arc-end a1) (arc-end a2))))
  
  ) ; END module 'event'
