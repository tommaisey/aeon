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
   /- in* in:
   to: to+ to- to* to/ to?
   mv+ mv- mv* mv/
   rp: tr: tr? cp: cp?
   is? any-of all-of none-of phrase)

  (import
    (chezscheme)
    (for (subdivide) expand)
    (utilities) (event) (context) (node-eval)
    (chain-nodes) (value-nodes) (srfi s26 cut))

  ;; A node that adds blank events according to a subdividing pattern.
  (define (in* pdef . ops)
    (apply o-> (lambda (c) (dispatch-pdef pdef c in*impl)) ops))

  ;; A node that adds events with a single specified property.
  (define (in: key pdef . ops)
    (apply o-> (lambda (c) (dispatch-pdef pdef c (in:impl key))) ops))

  ;; A node that replaces the input with the result of applying
  ;; it to each pattern member, which must all be functional nodes.
  (define (rp: pdef)
    (lambda (context)
      (dispatch-pdef pdef context rp:impl)))
  
  ;; A node that sets a property of events according to the pattern.
  ;; key value ... -> (context -> context)
  (define (to: . kv-pairs)
    (apply x-> (kv-pairs-to-nodes kv-pairs to:impl)))

  ;; A general 'to', taking a math op, a key and a def. The math op is
  ;; called with the current value for key and the value returned by def.
  (define (to math-op . kv-pairs)
    (apply x-> (kv-pairs-to-nodes kv-pairs (lambda (key) (to-math-impl math-op key)))))

  (define (to+ . kv-pairs) (apply to + kv-pairs))
  (define (to- . kv-pairs) (apply to - kv-pairs))
  (define (to* . kv-pairs) (apply to * kv-pairs))
  (define (to/ . kv-pairs) (apply to / kv-pairs))

  ;; A general 'mv', taking a math op and a def. The math op is
  ;; called with each segment's current time and the value returned by
  ;; def. The input context is resolved with a different arc, in effect
  ;; shifting it in time.
  (define (mv math-op inv-math-op . pdefs)
    (apply x-> (pdefs-to-nodes pdefs (mv-math-impl math-op inv-math-op))))

  (define (mv+ . pdefs) (apply mv + - pdefs))
  (define (mv- . pdefs) (apply mv - + pdefs))
  (define (mv* . pdefs) (apply mv * / pdefs))
  (define (mv/ . pdefs) (apply mv / * pdefs))

  ;;---------------------------------------------------------------
  ;; Composite chaining operators. Starting to get the feeling that
  
  ;; Transforms each event and returns the transformed copies only.
  (define (tr: . nodes)
    (lambda (context)
      (render (apply x-> nodes) context)))

  ;; Same as tr:, but only events matching pred are returned.
  ;; Confusing? People may expact this to replace only events
  ;; matching pred, and to let the others through.
  (define (tr? pred . nodes)
    (lambda (context)
      (context-filter pred ((apply tr: nodes) context))))
  
  ;; Same as tr:, but returns both the copies and the originals.
  (define (cp: . nodes)
    (lambda (context)
      (contexts-merge ((apply tr: nodes) context)
		      (context-resolve context))))

  ;; Same as tr?, but returns both the filtered copies and the originals.
  (define (cp? pred . nodes)
    (lambda (context)
      (contexts-merge ((apply tr? pred nodes) context)
		      (context-resolve context))))

  ;; Like cm?, but merges via the predicate. Returns unaltered
  ;; those events which fail the predicate.
  (define (to? pred . nodes)
    (lambda (context)
      (let* ([resolved (context-resolve context)]
	     [unfiltered (context-filter (lambda (c) (not (pred c))) resolved)])
	(contexts-merge unfiltered ((apply tr? pred nodes) context)))))

  ;; Produces a 'delay line', though a stateless one (i.e. changing
  ;; the source affects the line instantly).
  ;; Both beats-per-tap and num-taps may be pdefs.
  ;; iterative-nodes are successivly applied to the taps.

  ;; Hmm, fuck, old dispatch-pdef may not fit!
  (define (echo beats-per-tap num-taps . iterative-nodes)
    (lambda (context)
      (let ([impl (echo-impl num-taps iterative-nodes)])
	(dispatch-pdef beats-per-tap context impl))))
  
  ;;---------------------------------------------------------
  ;; Predicates & filtering. WARNING: This is all quite dated,
  ;; may need to change as it hasn't been tested in a while.

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

  ;;-------------------------------------------------------------------
  ;; Helper for building nodes from a list of pdefs
  ;; (pdef ...), impl -> ((context -> context) ...)
  (define (pdefs-to-nodes pdefs impl)
    (map (lambda (p) (lambda (c) (dispatch-pdef p c impl))) pdefs))

  ;; Helper for building a list of nodes from pairs of keys and values,
  ;; used in the implementation of the to: family of ops above.
  ;; (key value ...), (key -> impl) -> ((context -> context) ...)
  (define (kv-pairs-to-nodes pairs impl)
    (define (make-node key val)
      (lambda (c) (dispatch-pdef val c (impl key))))
    (if (zero? (mod (length pairs) 2))
	(let loop ([pairs pairs] [out '()])
	  (if (null? pairs)
	      (reverse out)
	      (loop (cddr pairs)
		    (cons (make-node (car pairs) (cadr pairs)) out))))
	(raise (string-append
		"'to' family operators must have an even number of arguments, "
		"made up of alternating keys and patterns."))))

  ) ; end module 'logic-nodes'
