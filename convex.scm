;; -*- geiser-scheme-implementation: chez-*-
;; requires thunderchez in library-directories.
(library-directories '("." "~/Code/scheme-libs/thunderchez"))
(optimize-level 2)
(import (rnrs))
(import (utilities))
(import (note))
(import (rhythm))
(import (note-dsl))
