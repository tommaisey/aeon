;; -*- geiser-scheme-implementation: chez-*-
(compile-imported-libraries #t)
(library-extensions (list ".chezscheme.sls"
			  ".sc"
			  ".ss"
			  ".scm"
			  ".sls"
			  ".sch"))
(library-directories (list "."
			   "~/Code/convex-deps"
			   "~/Code/convex-deps/thunderchez"
			   "~/Code/convex-deps/rsc3"))
(optimize-level 2)

(define-syntax recompile-import
  (syntax-rules ()
    ((_ lib-name)
     (begin
       (maybe-compile-library (string-append (symbol->string 'lib-name) ".scm"))
       (import (lib-name))))))

(import (scheme))
(import (rsc3))
(import (sosc))
(import (rhs))
(recompile-import utilities)
(recompile-import event)
(recompile-import context)
(recompile-import auto-quasi)
(recompile-import leaf)
(recompile-import branch)
(recompile-import trunk)
(recompile-import playback)

;;-----------------------------------------------------
;; Stop a mutex in the innards of Chez from blocking on Emacs REPLs.
;; This prevents our background thread from running in Emacs.
(define (fix-emacs-repl)
  (let ([stdin (transcoded-port (standard-input-port)
				(make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

(fix-emacs-repl)
