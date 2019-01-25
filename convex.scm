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
	(context)
	(rsc3)
	(sosc)
	(rhs)) ;; rhs for use in rsc3 examples

;; TODO: check that SuperCollider is running. I think I'll need
;; to implement select(2) in (sockets ffi) and (sockets syntax),
;; then expose that from the sosc/ext/ip.chez.scm udp interface.

;; Some SuperCollider test sounds...
(define (sine-perc freq time)
  (let* ([d (env-perc 0.01 time 1 (list -4 -4))]
	 [e (env-gen kr 1 0.1 0 1 remove-synth d)])
    (audition (out 0 (mul e (sin-osc ar freq 0))))))

(define (sine-smooth freq time)
  (let* ([d (env-sine time 0.1)]
	 [e (env-gen kr 1 0.2 0 1 remove-synth d)])
  (audition (out 0 (mul e (sin-osc ar freq 0))))))

