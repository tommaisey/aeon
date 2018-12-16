;; -*- geiser-scheme-implementation: chez-*-
;; ------------------------------------------------------------
;; Macros/functions that implement a DSL for specifying/transforming
;; musical patterns. We don't want beginners to ever have to type
;; 'lambda' or scary words like that. We'd prefer that they don't
;; know they're programming at all.
;; ------------------------------------------------------------

(module note-dsl
    (to
     with
     has
     any)

  (import note)

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

  ;; Logical note operators. Take a note and N [key pred arg] lists.
  ;; Checks if all/any values at the keys match the predicates.
  ;; Optionally partially applied.
  (define-syntax has
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (n) (and
		    (note-has n 'key)
		    (pred (note-get n 'key #f) args ...)
		    ((has rest ...) n))))

      ;; assume any other form is a lambda. allows nesting
      ((_ pred rest ...)
       (lambda (n) (and (pred n) ((has rest ...) n))))

      ((base-case) (lambda (x) #t))
      ((direct-call note pairs ...) ((has pairs ...) note))))

  (define-syntax any
    (syntax-rules ()
      
      ((_ [key pred args ...] rest ...)
       (lambda (n) (or
		    (and (note-has n 'key)
			 (pred (note-get n 'key #f) args ...))
		    ((any rest ...) n))))

      ;; assume any other form is a lambda. allows nesting
      ((_ pred rest ...)
       (lambda (n) (or (pred n) ((any rest ...) n))))

      ((base-case) (lambda (x) #f))
      ((direct-call note pairs ...) ((any pairs ...) note))))

  ) ; end module 'note dsl'
