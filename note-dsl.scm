;; -*- geiser-scheme-implementation: chez-*-
;; ------------------------------------------------------------
;; Macros/functions that implement a DSL for specifying/transforming
;; musical patterns. We don't want beginners to ever have to type
;; 'lambda' or scary words like that. We'd prefer that they don't
;; know they're programming at all.
;; ------------------------------------------------------------

(library (note-dsl)
  (export to with has any)

  (import (chezscheme) (note))

  ;; Returns unary lambda or allows a direct call with a note.
  ;; Gives this syntax for setting properties of a note:
  ;;
  ;; (to note
  ;;  [amp 0.6]
  ;;  [pan (random -0.3 0.3)]
  ;;  [freq (* input 2) 440]) ; 440 is the default for `input`
  ;;
  ;; Within a setting form, the special keyword `input` gets the
  ;; current value. A third form supplies a default for `input` if
  ;; the property doesn't exist - it'll be 0 if there's no default.
  (define-syntax to
    (lambda (x)
      (syntax-case x ()
	
	((_ (key value default) rest ...)
	 (with-syntax ([input (datum->syntax (syntax key) 'input)])
	   (syntax (lambda (note)
		     (let ([input (note-get note 'key default)])
		       (note-set! note 'key value))
		     ((to rest ...) note)))))

	((default-0 (key value) rest ...)
	 (syntax (to (key value 0) rest ...)))
	
	((base-case)
	 (syntax (lambda (note) note)))

	((direct-call note rest ...)
	 (syntax ((to rest ...) note)))

	((_ ...)
	 (syntax-error
	  "'to' should contain a series of key/value pairs.")))))

  ;; with is an alias for 'to'. TODO: should return a new note?
  (define-syntax with
    (syntax-rules () ((_ rest ...) (to rest ...))))

  ;; Logical note filters. Take a note list and N [key pred arg] lists.
  ;; Checks if all/any values at the keys match the predicates.
  ;; When called with a single note they return #t/f# like a normal
  ;; predicate. When called without that they return a lambda that
  ;; filters a list of notes.
  ;;
  ;; Problems:
  ;; - Brittle, no nice errors
  ;; - Don't allow nesting
  ;; - Related: don't allow injecting custom predicates
  (define-syntax has-test
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (n)
	 (and
	  (note-check n 'key pred args ...)
	  ((has-test rest ...) n))))

      ((direct-call note exprs ...) ((has-test exprs ...) note))
      ((base-case) (lambda (x) #t))))

  (define-syntax any-test
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (n)
	 (or
	  (note-check n 'key pred args ...)
	  ((any-test rest ...) n))))

      ((direct-call note exprs ...) ((any-test exprs ...) note))
      ((base-case) (lambda (x) #f))))

  ;; Main versions return functions to filter a note list.
  (define-syntax has
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (note-list)
	 (filter (has-test [key pred args ...] rest ...) note-list)))))

  (define-syntax any
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (note-list)
	 (filter (any-test [key pred args ...] rest ...) note-list)))))

  ) ; end module 'note dsl'
