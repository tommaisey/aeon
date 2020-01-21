;; Defines some tools for making a (somewhat) interactive documentation
;; system. The system is intended for use by musicians, not those who
;; are hacking on the system itself - so I'll still use comments for
;; 'internal' code.
(library (doc)
  (export make-docs-dict
          make-doc
          print-doc
          get-doc-desc 
          get-doc-args
          get-doc-examples)
  (import (chezscheme) (utilities))

  ;; Make a dictionary of documentation data.
  (define (make-docs-dict)
    (make-eq-hashtable 32))

  ;; For use inside libraries so they can expose a hook to add their
  ;; documentation to a common docs-dict. Defines a function which
  ;; adds the specified documentation to a docs-dict.
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

  ;; Get documentation data for a symbol out of a docs-dict.
  (define (get-doc docs-dict fn-sym)
    (hashtable-ref docs-dict fn-sym #f))

  ;; Call a function on some documentation data for a symbol, or
  ;; print a 'no docs' message if it's not found in the docs-dict.
  (define (do-doc docs-dict fn-sym fn)
    (lest [d (get-doc docs-dict fn-sym)]
          (fn d)
          (println (string-append "No docs for '" (symbol->string fn-sym) "' yet."))))

  (define (print-doc docs-dict fn-sym)
    (define (print d)
      (let* ([port (current-output-port)]
             [divider "-----------------------------\n"]
             [args (cadr d)]
             [examples (caddr d)]
             [desc (car d)]
             [name (symbol->string fn-sym)]
             [name-quot (str+ "'" name "'")])
        (fresh-line port)
        (newline port)
        (newline port)
        (println "=============================\n")
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
