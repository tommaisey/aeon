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
			   "~/Code/scheme-libs"
			   "~/Code/scheme-libs/thunderchez"
			   "~/Code/scheme-libs/rsc3"))
(optimize-level 2)

(import (rnrs)
	(utilities)
	(note)
	(rhythm)
	(note-dsl)
	(pattern-node)
	(context)
	(rsc3)
	(sosc))

