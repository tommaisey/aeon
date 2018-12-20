;; -*- geiser-scheme-implementation: chez-*-
;;-----------------------------------------------------------------
;; Building pipelines of note transformations.
;;-----------------------------------------------------------------
(library (note-pipeline)
  (export
   make-context
   context?
   context-notes
   context-window
   print-context

   change-if
   copy-if
   change-all
   copy-all
   pattern)

  (import (chezscheme) (utilities) (note))

  ;; A context to be passed to a notes pipeline function - the
  ;; pipeline must add to/transform the notes list.
  (define-record-type context
    (fields (mutable notes)
	    (mutable window)))

  (define (print-context c)
    (display "start: ")
    (display (window-start (context-window c)))
    (display ", end: ")
    (display (window-end (context-window c)))
    (newline)
    (print-notes (context-notes c)))

  (define-unary (change-if context filter-fn update-fn!)
    (rec-set
     [notes context context-notes context-notes-set!]
     (for-each update-fn! (filter-fn notes))))

  (define-unary (copy-if context filter-fn mutate-fn)
    (rec-set
     [notes context context-notes context-notes-set!]
     (append notes
      (map (lambda (n) (mutate-fn (note-copy n)))
	   (filter-fn notes)))))

  (define-unary (change-all context mutate-fn)
    (change-if context (lambda (x) #t) mutate-fn))

  (define-unary (copy-all context mutate-fn)
    (copy-if context (lambda (x) #t) mutate-fn))

  (define (merge-pattern-results ll)
    (define (cmp n1 n2)
      (< (note-get n1 'beat 0)
	 (note-get n2 'beat 0)))
    (merge-in-order (map (lambda (l) (sort! cmp l)) ll)))

  (define (pattern . filter-fns)
    (lambda (context)
      (let* ([do-filter (lambda (f) (f (context-notes context)))]
	     [matches (map do-filter filter-fns)]
	     [num (length matches)])
	(if (< num (length filter-fns))
	    (list)
	    (merge-pattern-results matches)))))

  ) ; end module 'note-pipeline'
