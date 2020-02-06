(compile-imported-libraries #t)
(optimize-level 2)

(library-extensions 
 (list ".chezscheme.sls" ".sc" ".ss" ".scm" ".sls" ".sch"))

(library-directories
 (list "."
       "./libs"
       "./libs/tests"
       "./libs/third-party"
       "./libs/third-party/thunderchez"
       "./libs/third-party/sc3"))

(import (scheme)
        (prefix (rsc3) sc/)
        (prefix (sosc) so/)
        (utilities)
        (doc)
        (file-tools)
        (event)
        (arc)
        (context)
        (pdef)
        (node-eval)
        (nodes-leafs)
        (nodes-subdivide)
        (nodes-chains)
        (nodes-ops)
        (nodes-sugar)
        (nodes-ops-time)
        (nodes-ops-chords)
        (midinotes)
        (harmony)
        (rhythm)
        (playback)
        (samples)
        (synthesis))
; (import (controls)) ;; I'll come back to this

;;------------------------------------------------------------------
;; Pull docs from libraries that expose them, add them to a
;; global dict. Also define convenience macros for accessing.
(define docs-dict (make-docs-dict))
(for-each (lambda (f) (f docs-dict))
          (list rhythm-docs
                harmony-docs
                nodes-ops-docs
                subdivide-docs
                chain-docs))

(define-syntax doc
  (syntax-rules () ((_ name) (print-doc docs-dict 'name))))
(define-syntax docstr
  (syntax-rules () ((_ name) (get-doc-desc docs-dict 'name))))
(define-syntax docargs
  (syntax-rules () ((_ name) (get-doc-args docs-dict 'name))))
(define-syntax docex
  (syntax-rules () ((_ name) (get-doc-examples docs-dict 'name))))

;;------------------------------------------------------------------
(define (run-tests)
  (load "libs/tests/harmony-tests.scm")
  (load "libs/tests/basic-ops-tests.scm")
  (load "libs/tests/time-ops-tests.scm"))

;; Uncomment to run tests on engine init.
;; (run-tests)