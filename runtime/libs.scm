(compile-imported-libraries #t)
(optimize-level 2)

(define aeon-dir
  (if (top-level-bound? 'aeon-file)
      (path-parent aeon-file)
      (current-directory)))

(library-extensions
 (list ".chezscheme.sls" ".sc" ".ss" ".scm" ".sls" ".sch"))

(library-directories
 (map (lambda (s) (string-append aeon-dir s))
      (list  "/libs"
             "/libs/third-party"
             "/libs/third-party/thunder"
             "/libs/third-party/sc3"
             "/tests")))

(import (scheme)
        (utilities)
        (file-tools)
        (system)
        (rename (version-control) (jump vc:jump)))

(build-aeon-libraries aeon-dir)

(import (prefix (rsc3) sc/)
        (prefix (sosc) so/)
        (rename (matchable) (? ??) ($ $$))
        (project)
        (doc)
        (event)
        (arc)
        (context)
        (context-render)
        (seq-def)
        (seq-continuous)
        (seq-subdivide)
        (seq-eval)
        (ops-basic)
        (ops-chains)
        (ops-sugar)
        (ops-time)
        (ops-chords)
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
                ops-basic-docs
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
