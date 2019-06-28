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
(import (rsc3))
(import (sosc))
(import (rhs))
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
(import (midinotes))
(import (harmony))
(import (playback))
(import (samples))