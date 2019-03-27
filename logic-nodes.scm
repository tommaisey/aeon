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
   in* in: to: to+ to- to* to/ to?
   rp: tr: tr? cp: cp? ch:
   is? any-of all-of none-of phrase)

  (import
    (chezscheme)
    (for (pdef) expand)
    (for (subdivide) expand)
    (utilities) (event) (context) (node-eval)
    (chain-nodes) (value-nodes) (srfi s26 cut))
  
  ;; A node that sets a property of events according to a subdividing pattern.
  (define-syntax to:
    (syntax-rules ()

      ((_ key def)
       (to: key 1 def))
      
      ((_ key dur def)
       (lambda (context)
	 (to:impl context dur (pdef def) key)))))

  ;; A general 'to', taking a math op, a key and a def. The math op is
  ;; called with the current value for key and the value returned by def.
  (define-syntax to
    (syntax-rules ()

      ((_ math-op key def)
       (to math-op key 1 def))
      
      ((_ math-op key dur def)
       (lambda (context)
	 (to-math-impl math-op context dur (pdef def) key)))))
  
  (define-syntax to+
    (syntax-rules () ((_ x ...) (to + x ...))))
  (define-syntax to-
    (syntax-rules () ((_ x ...) (to - x ...))))
  (define-syntax to*
    (syntax-rules () ((_ x ...) (to * x ...))))
  (define-syntax to/
    (syntax-rules () ((_ x ...) (to + x ...))))

  ;; A node that adds blank events according to a subdividing pattern. 
  (define-syntax in*
    (syntax-rules ()

      ((_ def)
       (in* 1 def))

      ((_ dur def (to-key r ...) ...)
       (lambda (context)
	 (let* ([c (in*impl context dur (pdef def))]
		[c ((to: to-key r ...) c)] ...)
	   (contexts-merge context c))))))

  ;; A node that adds events with a single specified property.
  (define-syntax in:
    (syntax-rules ()

      ((_ key def)
       (in: key 1 def))

      ((_ key dur def (to-key r ...) ...)
       (lambda (context)
	 (let* ([c (in:impl key context dur (pdef def))]
		[c ((to: to-key r ...) c)] ...)
	   (contexts-merge context c))))))

  ;; A node that replaces the input with the result of applying
  ;; it to each pattern member, which must all be functional nodes.
  (define-syntax rp:
    (syntax-rules ()

      ((_ def)
       (rp: 1 def))

      ((_ dur def)
       (lambda (context)
	 (rp:impl context dur (pdef def))))))

  ;;---------------------------------------------------------------
  ;; Composite chaining operators. Starting to get the feeling that
  
  ;; Transforms each event and returns the transformed copies only.
  (define (tr: . cnodes)
    (lambda (context)
      (render (apply x-> cnodes) context)))

  ;; Same as tr:, but only events matching pred are returned.
  ;; Confusing? People may expact this to replace only events
  ;; matching pred, and to let the others through.
  (define (tr? pred . cnodes)
    (lambda (context)
      ((apply tr: cnodes) (context-filter pred context))))
  
  ;; Same as tr:, but returns both the copies and the originals.
  (define (cp: . cnodes)
    (lambda (context)
      (contexts-merge context ((apply tr: cnodes) context))))

  ;; Same as tr?, but returns both the filtered copies and the originals.
  (define (cp? pred . cnodes)
    (lambda (context)
      (contexts-merge context ((apply tr? pred cnodes) context))))

  ;; Applies each op to the original context independently, and returns
  ;; all the results as well as the originals. Works best with tr, tr?
  (define (ch: . ops)
    (lambda (context)
      (fold-left (lambda (c op) (contexts-merge c ((tr: op) context))) context ops)))

  ;; Like cm?, but merges via the predicate. The returned list contains
  ;; unaltered notes
  (define (to? pred . cnodes)
    (lambda (context)
      (let* ([unfiltered (context-filter (lambda (c) (not (pred c))) context)])
	(contexts-merge unfiltered ((apply tr? pred cnodes) context)))))
  
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
