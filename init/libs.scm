(compile-imported-libraries #t)
(optimize-level 2)

(library-extensions 
 (list ".chezscheme.sls" ".sc" ".ss" ".scm" ".sls" ".sch"))

(library-directories
 (list "."
       "./libs"
       "./libs/third-party"
       "./libs/third-party/thunderchez"
       "./libs/third-party/rsc3"))

(import (scheme))
(import (rsc3))
(import (sosc))
(import (rhs))
(import (utilities))
(import (event))
(import (context))
(import (pdef))
(import (value-nodes))
(import (basic-nodes))
(import (chain-nodes))
(import (time-nodes))
(import (node-eval))
(import (harmony))
(import (playback))
(import (samples))
