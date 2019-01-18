
;; -*- geiser-scheme-implementation: chez-*-
;; requires thunderchez in library-directories.
(compile-imported-libraries #t)
(library-directories '("." "~/Code/scheme-libs/thunderchez"))
(optimize-level 2)
(import (rnrs))
(import (utilities))
(import (note))
(import (rhythm))
(import (note-dsl))
(import (pattern-node))

;; Imports for repl testing - shouldn't be public
(import (context))
