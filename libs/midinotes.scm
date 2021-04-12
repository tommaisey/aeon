
;; Procedural macro that defines a library. Each of the input name -> value pairs
;; will have definitions created for each midi octave with the value adjusted by 12.
(define-syntax def-octaved
  (lambda (x)

    (define numbers (map (lambda (x) x) (iota 7)))

    (define (octavize name value)
      (define (n->n+v i)
        (list (string->symbol (string-append
                               (symbol->string name)
                               (number->string i)))
              (+ 24 (* i 12) value)))
      (cons (list name (+ 60 value)) (map n->n+v numbers)))

    (define (make-names k names-values-list)
      (define (build r x) (append (octavize (car x) (cadr x)) r))
      (datum->syntax k (fold-left build '() (syntax->datum names-values-list))))

    (syntax-case x ()
      ((k lib-name (name value) ...)
       (with-syntax ((((n v) ...) (make-names #'k #'((name value) ...))))
         #'(library (lib-name)
             (export n ...)
             (import (chezscheme))
             (define n v) ...))))))

;; Aaand here's the library definition
(def-octaved midinotes
 (C 0)  (Cs 1) (Db 1)  (D 2)   (Ds 3)  (Eb 3)
 (E 4)  (F 5)  (Fs 6)  (Gb 6)  (G 7)   (Gs 8)
 (Ab 8) (A 9)  (As 10) (Bb 10) (B 11))
