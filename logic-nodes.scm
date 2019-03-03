;; -*- geiser-scheme-implementation: chez-*-
;; ------------------------------------------------------------
;; branches
;;
;; Functions that take contexts and return new contexts.
;; These form the backbone of our system, doing all the
;; transformations on contexts that make our musical patterns.
;; The cnodes can be chained and combined with fns in @trunk.
;; The 'leaves' of our system are described in @leaf.
;; ------------------------------------------------------------
(library (logic-nodes)
  (export
   in+ in± to! to? cp! cp? is?
   any-of all-of none-of phrase)

  (import
    (chezscheme)
    (for (auto-quasi) expand)
    (for (subdivide) expand)
    (utilities) (event) (context) (node-eval)
    (chain-nodes) (value-nodes) (srfi s26 cut))
  
  ;; A node that sets a property of events according to a subdividing pattern.
  (define-syntax to!
    (syntax-rules ()

      ((_ key pdef)
       (to! key 1 pdef))
      
      ((_ key pdur pdef)
       (lambda (context)
	 (to!impl context pdur (pdef-quasi pdef) key)))))

  ;; A node that adds blank events according to a subdividing pattern. 
  (define-syntax in+
    (syntax-rules ()

      ((_ pdef)
       (in+ 1 pdef))

      ((_ pdur pdef (:to-key r ...) ...)
       (lambda (context)
	 (let* ([c (in+impl context pdur (pdef-quasi pdef))]
		[c ((to! :to-key r ...) c)] ...)
	   (contexts-merge context c))))))

  ;; A node that adds events with a single specified property.
  (define-syntax in±
    (syntax-rules ()

      ((_ :key pdef)
       (in± :key 1 pdef))

      ((_ :key pdur pdef (:to-key r ...) ...)
       (lambda (context)
	 (let* ([c (in±impl :key context pdur (pdef-quasi pdef))]
		[c ((to! :to-key r ...) c)] ...)
	   (contexts-merge context c))))))

  ;; The implementation of these could be a lot better, but this
  ;; should get things working (to an extent I can use).
  (define (cp! . cnodes)
    (lambda (context)
      (let ([changed (render (apply x-> cnodes) context)])
	(contexts-merge context (context-trim changed)))))
  
  (define (cp? pred . cnodes)
    (lambda (context)
      (let* ([filtered (context-filter pred context)]
	     [changed (render (apply x-> cnodes) filtered)])
	(contexts-merge context (context-trim changed)))))

  (define (to? pred . cnodes)
    (lambda (context)
      (let* ([unfiltered (context-filter (lambda (c) (not (pred c))) context)]
	     [filtered (context-filter pred context)]
	     [changed (render (apply x-> cnodes) filtered)])
	(contexts-merge unfiltered (context-trim changed)))))
  
  ;;---------------------------------------------------------
  ;; Predicates & filtering.

  ;; Takes either a key (shorthand for (this key #f)) or a c-val.
  ;; WARNING: what if we want to check for values of #f?
  (define-syntax is?
    (syntax-rules ()
      ((_ key/getter pred args ...)
       (lambda (context)
	 (let ([v (if (procedure? key/getter)
		      (key/getter context)
		      ((this key/getter #f) context))])
	   (and v (pred v args ...)))))))
  
  ;; Find the intersection of the inner filters
  (define (all-of . preds)
    (lambda (context)
      ((combine-preds preds for-all) context)))

  ;; Find the union of the inner filters.
  (define (any-of . preds)
    (lambda (context)
      ((combine-preds preds for-any) context)))

  ;; Subtract the events matched by each filter from the input.
  (define (none-of . preds)
    (lambda (context)
      ((combine-preds preds for-none) context)))

  ;; Takes: a list of N filters (e.g. has, any)
  ;; Returns: a filter finding sequences matching the inputs.
  ;; This doesn't work very well currently. Will need thorough
  ;; unit tests.
  ;; TODO: not updated since switched to new context model...
  (define (phrase . filters)
    (define (merge-results ll)
      (let* ([columns  (map (cut sort event-before? <>) ll)]
	     [patterns (columns-to-rows columns)])
	(concatenate (filter (cut sorted? event-before? <>) patterns))))
    (lambda [events]
      (concatenate (map (cut <> events) filters))))

  ) ; end module 'cnodes'
