;; -*- geiser-scheme-implementation: chez-*-
;;-------------------------------------------------------------
;; A collection of data structures for representing note events
;; and their place in time. These can be transformed by the
;; special DSL in note-dsl.scm, or directly by users who don't
;; mind working directly in scheme.
;; ------------------------------------------------------------
(library (note)
  (export priority-keys
          make-note
	  note-get
	  note-set
	  note-update
	  note-check
	  note-optimise
	  note-prioritise
	  
	  note-beat
	  note-before?
	  print-note
	  print-notes
	  make-notes-with-times
	  make-notes-regular

	  window
	  window?
	  make-window
	  window-with-start
	  window-with-end
	  window-start
	  window-end
	  window-valid?
	  within-window?

	  context
	  make-context
	  context-notes
	  context-window
	  context-print
	  pipeline-node)

  (import (chezscheme) (utilities) (srfi s26 cut)
	  (only (srfi s1 lists) delete-duplicates))

  ;; A note event. It's really just an associative dictionary,
  ;; mapping keys to values. The most important key is 'beat'
  ;; which describes its position in time.
  ;; It's implemented as an immutable alist - every operation
  ;; returns a new list, the old one remains untouched.
  (define note-start-beat-key 'beat)
  (define priority-keys '(beat length freq))

  (define-syntax make-note
    (syntax-rules ()
      ((_ start-beat (key value) ...)
       (list (cons 'beat start-beat) (cons 'key value) ...))
      
      ((_ ...) (syntax-error "make-note should look like: (make-note 1/16 [freq 100]"))))

  (define (note-get note key default)
    (let ([result (assq key note)])
      (if result (cdr result) default)))
  (define (note-set note key value)
    (cons (cons key value) note))
  (define (note-update note key update-fn default)
    (note-set note key (update-fn (note-get note key default))))
  (define (note-beat n)
    (note-get n note-start-beat-key 0))
  (define (note-before? n1 n2)
    (< (note-beat n1) (note-beat n2)))
  ;; Checks there is an item at the key and that it
  ;; satisfies the predicate (which may have extra args)
  (define (note-check note key pred . args)
    (let ([v (note-get note key #f)])
      (when v (apply pred (cons v args)))))

  ;; note-optimise returns an identical-looking note that's
  ;; been cleaned of obsolete mappings and had certain key/value
  ;; pairs moved to the front to accelerate finding common entries.
  (define (note-prioritise note key)
    (let ([result (note-get note key #f)])
      (if (not result) note
	  (note-set note key result))))
  
  (define (note-optimise note)
    (let ([n (fold-left (cut note-prioritise <> <>) note priority-keys)])
      (delete-duplicates n (lambda (x y) (eq? (car x) (car y))))))

  ;; Some note convenience functions
  (define (print-note note)
    (display "[")
    (for-each
     (lambda (p)
       (display " ")
       (display (car p))
       (display ": ")
       (display (cdr p))
       (display ", "))
     (note-optimise note))
    (display "]")
    (newline))

  (define (print-notes note-list)
    (for-each print-note note-list))

  (define (make-notes-with-times times-list)
    (map (lambda (t) (make-note t)) times-list))

  (define (make-notes-regular num interval start)
    (define (impl lst num t)
      (if (= num 0) lst
	  (impl (cons t lst) (sub1 num) (+ t interval))))
    (make-notes-with-times (impl '() num start)))

  ;; A window of time (in beats)
  (define-record-type window
    (fields (immutable start)
	    (immutable end)))

  (define (window-with-start w new-start)
    (make-window new-start (window-end w)))
  (define (window-with-end w new-end)
    (make-window (window-start w) new-end))
  (define (window-valid? w)
    (< (window-start w) (window-end w)))
  (define (within-window? w t)
    (between t (window-start w) (window-end w)))

  ;; Find the window which a list of notes encompasses.
  ;; The call to list-last makes this relatively slow.
  (define (window-from-notes notes)
    (apply make-window
     (if (null? notes)
	 (list 0 0)
	 (list (note-beat (car notes))
	       (note-beat (list-last notes))))))

  ;; A context containing a note list and a window of time
  ;; that transformations should care about.
  (define-record-type context
    (fields (immutable notes)
	    (immutable window)))

  (define (context-print c)
    (let ([win (context-window c)])
      (display "Window: ")
      (display (window-start win))
      (display ", ")
      (display (window-end win))
      (newline)
      (print-notes (context-notes c))))

  ;; Produces a lambda that takes and returns a context.
  ;; The context is destructured and bound to the symbols
  ;; provided for its notes and/or window. Use:
  ;;
  ;; (pipeline-node [notes win] (body returning context))
  ;; (pipeline-node [notes] (body returning notes)
  (define-syntax pipeline-node
    (syntax-rules ()

      ;; Binds context's notes and window to supplied symbols.
      ;; Should return a transformed context.
      ((_ [notes-id window-id] body rest ...)
       (lambda (context)
	 (let ([notes-id (context-notes context)]
	       [window-id (context-window context)])
	   body rest ...)))

      ;; Binds a context's notes to the supplied symbol,
      ;; or accepts and rebinds a bare list of notes.
      ;; `body` should return a notes list, but the lambda
      ;; will return a context.
      ((_ [notes-id] body rest ...)
       (lambda (ctx/notes)
	 (cond
	  ((context? ctx/notes)
	   (let ([notes-id (context-notes ctx/notes)]
		 [window (context-window ctx/notes)])
	     (make-context (begin body rest ...) window)))

	  ((list? ctx/notes)
	   (let ([notes-id ctx/notes])
	     (begin body rest ...)))

	  (else
	   (begin
	     (display ctx/notes)
	     (raise "pipeline-node only binds to contexts or note lists"))))))

      ((_ ...)
       (syntax-error "pipeline-node syntax error."))))

  ) ; end module 'note'
