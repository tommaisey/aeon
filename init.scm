;; -*- geiser-scheme-implementation: chez-*-
;; requires thunderchez in library-directories.
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

(import (rnrs)
	(utilities)
	(note)
	(rhythm)
	(note-dsl)
	(pattern-node)
	(subdivide)
	(c-vals)
	(context)
	(playback)
	(rsc3)
	(sosc)
	(rhs))

;;-----------------------------------------------------
;; Stop a mutex in the innards of Chez from blocking on Emacs REPLs.
;; This prevents our background thread from running in Emacs.
(define (fix-emacs-repl)
  (let ([stdin (transcoded-port (standard-input-port)
				(make-transcoder (utf-8-codec)))])
    (current-input-port stdin)
    (console-input-port stdin)))

(fix-emacs-repl)

;;------------------------------------------------------
;; TODO: check that SuperCollider is running, issue a warning if not.
;; As per sclang put the default server into variable named 's'
(define s (udp:open "127.0.0.1" 57110))
(send s (g-new1 1 add-to-tail 0))
