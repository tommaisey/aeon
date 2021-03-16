(import (rnrs) (mk-r6rs))

(define rhs-libraries
  (map
   (lambda (x)
     (string-append "../src/" x))
   (list "prelude.scm"
         "control/monad.scm"
         "control/monad.syntax.scm"
         "data/bool.scm"
         "data/bool.syntax.scm"
         "data/function.scm"
         "data/list.scm"
         "data/ord.scm"
         "data/tree.scm"
         "data/tuple.scm")))

;; bindings required to compile rhs
(define rhs-requires
  (quote
   (quote
    define
    define-record-type fields
    define-syntax syntax-rules
    lambda
    let let* letrec
    if cond else
    cons car cdr pair? list
    equal?
    = + - * /
    and or
    even? odd?
    > < >= <=
    error
    )))

;; bindings rhs introduces but ought not to export
(define rhs-private
  (quote
   (mergesort mergesort* merge-pairs merge)))

;; equalities with the (rnrs) library
(define rnrs-equalities
  '(append
    filter
    find
    length
    list-ref
    map
    min max
    not
    null?
    reverse))

(define r6rs-dir (list-ref (command-line) 1))

(mk-r6rs '(rhs)
         rhs-libraries
         (string-append r6rs-dir "/rhs.sls")
         `((only (rnrs) ,@rhs-requires)
           (prefix (srfi s9 records) srfi:))
         rhs-private
         rnrs-equalities)

; (mk-r7rs '(rhs)
;          rhs-libraries
;          (string-append r7rs-dir "/rhs.sld")
;          `((only (scheme base) ,@rhs-requires)
;            (prefix (srfi 9) srfi:))
;          rhs-private
;          rnrs-equalities)

