;; -*- geiser-scheme-implementation: chez-*-
;;-----------------------------------------------------------------
;; Building pipelines of note transformations.
;;-----------------------------------------------------------------
(module note-pipeline
    (context?
     make-context
     context-notes
     context-window
     print-context

     change-if
     copy-if
     change-all
     copy-all)

  (import utilities)
  (import note)

  ;; A context to be passed to a notes pipeline function - the
  ;; pipeline must add to/transform the notes list.
  (define-record context (notes window))

  (define (print-context c)
    (display "start: ")
    (display (window-start (context-window c)))
    (display ", end: ")
    (display (window-end (context-window c)))
    (newline)
    (print-notes (context-notes c)))

  (define-unary (change-if context match-fn update-fn!)
    (rec-set
     [notes context context-notes set-context-notes!]
     (for-each
      (lambda (n)
	(when (match-fn n)
	  (update-fn! n)))
      notes)
     notes))

  (define-unary (copy-if context match-fn mutate-fn)
    (rec-set
     [notes context context-notes set-context-notes!]
     (define (impl in out)
       (if (null? in) out
	   (impl (cdr in)
		 (if (not (match-fn (car in))) out		     
		     (cons (mutate-fn (note-copy (car in))) out)))))
     (append (impl notes '()) notes)))

  (define-unary (change-all context mutate-fn)
    (change-if context (lambda (x) #t) mutate-fn))

  (define-unary (copy-all context mutate-fn)
    (copy-if context (lambda (x) #t) mutate-fn))

  ) ; end module 'note-pipeline'
