;; -*- geiser-scheme-implementation: chez-*-
;; ------------------------------------------------------------
;; Macros/functions that implement a DSL for specifying/transforming
;; musical patterns. We don't want beginners to ever have to type
;; 'lambda' or scary words like that. We'd prefer that they don't
;; know they're programming at all.
;;
;; DSL basically consists of two parts: filters and transformers.
;;
;; Filters return lambdas taking a context (or, for repl convenience,
;; a raw note list - see pipeline-node) and returning a new one
;; with a filtered note list. While it would be simpler if it returned
;; a lambda taking an individual note, we want filters to be able to
;; recognise sequential patterns of notes.
;;
;; Transformers also take a context/note-list, and do something to
;; each of the notes, returning a new list.
;; ------------------------------------------------------------

(library (note-dsl)
  (export to with has any none phrase
	  change-all copy-all
	  change-if copy-if)

  (import (chezscheme) (note) (utilities) (srfi s26 cut))

  ;; Macro for checking that the properties of a note
  ;; match a certain (partially applied) predicate.
  (define-syntax has
    (syntax-rules ()
      ((has key pred args ...)
       (pipeline-node [notes]
	 (filter (cut note-check <> 'key pred args ...) notes)))))

  ;; Find the intersection of the inner filters
  (define (all . filters)
    (pipeline-node [notes]
      (let impl ([fns filters] [nts notes])
	(cond
	 ((or (null? nts) (null? fns)) nts)
	 (else (impl (cdr fns) ((car fns) nts)))))))

  ;; Find the union of the inner filters.
  (define (any . filters)
    (pipeline-node [notes]
      (let impl ([fns filters] [nts notes] [out '()])
	(cond
	 ((or (null? nts) (null? fns)) out)
	 (else (let ([found ((car fns) nts)])
		 (impl (cdr fns)
		       (remove-list nts found)
		       (append found out))))))))

  ;; Subtract the notes matched by each filter from the input.
  (define (none . filters)
    (pipeline-node [notes]
      (let impl ([fns filters] [nts notes])
	(cond
	 ((or (null? nts) (null? fns)) nts)
	 (else (let ([found ((car fns) nts)])
		 (impl (cdr fns) (remove-list nts found))))))))

  ;; Takes: a list of N filters (e.g. has, any)
  ;; Returns: a filter finding sequences matching the inputs.
  ;; This doesn't work very well currently. Will need thorough
  ;; unit tests.
  (define (phrase . filters)
    (define (merge-results ll)
      (let* ([columns  (map (cut sort note-before? <>) ll)]
	     [patterns (columns-to-rows columns)])
	(merge-inner (filter (cut sorted? note-before? <>) patterns))))
    (pipeline-node [notes]
      (merge-inner (map (cut <> notes) filters))))

  ;;-----------------------------------------------
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
  ;;
  ;; TODO: it's likely people will want to refer to the value of
  ;; other keys, e.g. [amp (* cutoff 0.01)]. How to do that?
  (define-syntax to
    (lambda (x)
      (syntax-case x ()

	((_ (key value default) rest ...)
	 (with-syntax ([input (datum->syntax (syntax key) 'input)])
	   (syntax
	    (lambda (note)
	      ((to rest ...)
	       (note-update note 'key
		(lambda (input) value) default))))))

	;; Because we're using alists, this version is much
	;; faster because it doesn't require a lookup to get
	;; the special 'input' value.
	((default-0 (key value) rest ...)
	 (with-syntax ([input (datum->syntax (syntax key) 'input)])
	   (syntax
	    (lambda (note)
	      ((to rest ...) (note-set note 'key value))))))
	
	((base-case)
	 (syntax (lambda (note) note)))

	((direct-call note rest ...)
	 (syntax ((to rest ...) note)))

	((_ ...)
	 (syntax-error
	  "'to' should contain a series of key/value pairs.")))))

  ;; 'with' is an alias for 'to'. TODO: should return a new note?
  (define-syntax with
    (syntax-rules () ((_ rest ...) (to rest ...))))

  ;;------------------------------------------------------
  ;; Top level transformation statements.
  (define (change-impl notes update-fn)
    (map update-fn notes))

  (define (change-all update-fn)
    (pipeline-node [notes]
      change-impl notes update-fn))

  (define (change-if filter-fn update-fn)
    (pipeline-node [notes]
      (change-impl (filter-fn notes) update-fn)))

  (define (copy-impl notes notes-to-copy update-fn)
    (append notes (map update-fn notes-to-copy)))

  (define (copy-all update-fn)
    (pipeline-node [notes]
      (copy-impl notes notes update-fn)))

  (define (copy-if filter-fn update-fn)
    (pipeline-node [notes]
      (copy-impl notes (filter-fn notes) update-fn)))

  ;; Adding statements
  (define (add to-add)
    (pipeline-node [notes]
      (append to-add notes)))

  (define (add-looped loop-window to-add)
    (pipeline-node [notes window]
      (make-context notes window))) ; TODO: stub

  ) ; end module 'note dsl'
