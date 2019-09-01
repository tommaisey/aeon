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
       "./libs/third-party/rsc3"))

(import (scheme))
        (prefix (rsc3) sc/)
        (prefix (sosc) so/)
(import (utilities))
(import (file-tools))
(import (event))
(import (context))
(import (pdef))
(import (node-eval))
(import (nodes-leafs))
(import (nodes-subdivide))
(import (nodes-chains))
(import (nodes-ops))
(import (nodes-ops-time))
(import (nodes-ops-chords))
; (import (controls)) ;; I'll come back to this
(import (midinotes))
(import (harmony))
(import (rhythm))
(import (playback))
(import (samples))

(tag-pdef-callable map)
(tag-pdef-callable filter)