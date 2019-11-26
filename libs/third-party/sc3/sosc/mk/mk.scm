(import (rnrs) (mk-r6rs))

(define sosc-src
  '("../src/bytevector.scm"
    "../src/sosc.scm"
    "../src/transport.scm"))

(mk-r6rs '(sosc)
         (cons "../src/ext/ip.chez.scm" sosc-src)
         (string-append (list-ref (command-line) 1) "/sosc.sls")
         '((rnrs) (rhs) (socket socket)) ;; https://github.com/theschemer/socket
         '()
         '())

(exit)
