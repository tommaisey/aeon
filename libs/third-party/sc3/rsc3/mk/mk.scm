(import (rnrs)
        (mk-r6rs))

(define rsc3-src
  '(;;"../src/alias.scm"
    "../src/command.scm"
    "../src/constant.scm"
    "../src/core.scm"
    "../src/envelope.scm"
    "../src/external.scm"
    "../src/hw.scm"
    "../src/hw/syntax.scm"
    "../src/list.scm"
    "../src/sc3.scm"
    "../src/ugen.scm"
    "../src/derived.scm"))

(define rsc3-dep
  '((rnrs)
    (rhs)
    (sosc)
    (prefix (srfi s9 records) srfi:)
    (prefix (srfi s19 time) srfi:)
    (prefix (srfi s27 random-bits) srfi:)))

(define chez-dep
  '(only 
    (chezscheme) 
    system
    sleep
    make-time
    time-nanosecond 
    time-second
    current-time))

(mk-r6rs '(rsc3)
	 (cons "../src/ext/sys.chez.scm" rsc3-src)
	 (string-append (list-ref (command-line) 1) "/rsc3.sls")
   (cons chez-dep rsc3-dep)
	 '()
	 '())

