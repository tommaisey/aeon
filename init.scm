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
	(context)
	(playback)
	(rsc3)
	(sosc)
	(rhs))

;;------------------------------------------------------
;; TODO: check that SuperCollider is running, issue a warning if not.
;; As per sclang put the default server into variable named 's'
(define s (udp:open "127.0.0.1" 57110))
(send s (g-new1 1 add-to-tail 0))
