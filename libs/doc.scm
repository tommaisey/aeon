(library (doc)
  (export make-docs-dict
          make-doc
          print-doc
          get-doc-desc 
          get-doc-args
          get-doc-examples)
  (import (chezscheme) (utilities))

  (define (make-docs-dict)
    (make-eq-hashtable 32))

  ;; Defines a function which, given a docs-dict, adds the specified
  ;; documentation data to the dictionary. Documentation is specified
  ;; in the format (fn-name "Doc string" (arg-name ArgType "Arg description")).
  ;; For use inside a library to expose a function adding docs to a docs-dict.
  (define-syntax make-doc
    (syntax-rules (=>)
      ((_ docs-fn-name 
          (name string
                ((arg type desc-str) ...)
                ((example => result) ...)) ...)
       (define (docs-fn-name docs-dict)
         (begin
           (when (hashtable-ref docs-dict 'name #f)
             (warning 'docs-fn-name "Overwriting pre-existing docs" 'name))
           (let* ([args (list (list 'arg 'type desc-str) ...)]
                  [examples (list (list 'example 'result) ...)])
             (hashtable-set! docs-dict 'name (list string args examples))))
         ...))))

  (define (get-doc docs-dict fn-sym)
    (hashtable-ref docs-dict fn-sym #f))

  (define (do-doc docs-dict fn-sym fn)
    (lest [d (get-doc docs-dict fn-sym)]
          (fn d)
          (println (string-append "No docs for '" (symbol->string fn-sym) "' yet."))))

  (define (print-doc docs-dict fn-sym)
    (define (print d)
      (let* ([port (current-output-port)]
             [top-divider "=============================\n"]
             [divider "-----------------------------\n"]
             [args (cadr d)]
             [examples (caddr d)]
             [desc (car d)]
             [name (symbol->string fn-sym)]
             [name-quot (str+ "'" name "'")])
        (fresh-line port)
        (newline port)
        (newline port)
        (println top-divider)
        (println (str+ "*** " name " ***"))
        (when (not (null? args))
          (let ([num-str (number->string (length args))])
            (newline port)
            (println (str+ name-quot " Needs (" num-str "):"))
            (println divider)
            (for-each (lambda (arg)
                        (put-datum port (car arg))
                        (display-string " | " port)
                        (put-datum port (cadr arg))
                        (display-string " | " port)
                        (display-string (caddr arg) port)
                        (newline port))
                      args)))
        (when (not (null? examples))
          (let ([num-str (number->string (length examples))])
            (newline port)
            (println (str+ name-quot " Examples (" num-str "):"))
            (println divider)
            (for-each (lambda (ex)
                        (put-datum port (car ex))
                        (display-string " => " port)
                        (put-datum port (cadr ex))
                        (newline port))
                      examples)))
        (newline port)
        (println (str+ name-quot " Description:"))
        (println divider)
        (println desc)))
    (do-doc docs-dict fn-sym print))

  (define (get-doc-desc docs-dict fn-sym)
    (do-doc docs-dict fn-sym car))

  (define (get-doc-args docs-dict fn-sym)
    (do-doc docs-dict fn-sym cadr))

  (define (get-doc-examples docs-dict fn-sym)
    (do-doc docs-dict fn-sym caddr))

  )
