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
        (prefix (sosc) so/))
(import (utilities))
(import (doc))
(import (file-tools))
(import (event))
(import (arc))
(import (context))
(import (pdef))
(import (node-eval))
(import (nodes-leafs))
(import (nodes-subdivide))
(import (nodes-chains))
(import (nodes-ops))
(import (nodes-ops-time))
(import (nodes-ops-chords))
(import (midinotes))
(import (harmony))
(import (rhythm))
(import (playback))
(import (samples))
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