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
       "./libs/third-party/sc3"))

(import (scheme)
        (prefix (rsc3) sc/)
        (prefix (sosc) so/))
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


(define (run-tests)
  (load "libs/tests/harmony-tests.scm")
  (load "libs/tests/basic-ops-tests.scm")
  (load "libs/tests/time-ops-tests.scm"))

;; Uncomment to run tests on engine init.
;; (run-tests)
