;; Exposes the 'subdivide' form. The actual subdivision algorithm is
;; implemented in 'impl-subdivide'. Note that this must be imported
;; (for (..) expand) to make it available at macroexpand time.
(library (subdivide)
  (export subdivide event)
  (import (scheme) (context) (for (impl-subdivide) expand))

  ;;------------------------------------------------------------------
  ;; Allows definition of nested lists without (quasi)quoting.
  ;; Lists that start with an identifier are evaluated.
  ;; Lists that don't start with an identifier are just a list.
  ;;
  ;; (auto-quasi (1 2 "hi" (+ 2 2) (5 (+ 5 5)))) => (1 2 "hi" 4 (5 10))
  (define-syntax auto-quasi
    (lambda (x)
      (syntax-case x ()
	((_ (v rest ...))
	 (identifier? (syntax v))
	 (syntax (v rest ...))) ; identifier first, call as lambda
	
	((_ (v ...))
	 (syntax (list (auto-quasi v) ...))) ;; make list
     
        ((_ v)
	 (syntax v))))) ;; normal value in a list

  ;;--------------------------------------------------------------------
  ;; The main subdivide macro. The special identifier 'event' marks a
  ;; statement that will add new events.
 
  (define event) ;; For export
  
  (define-syntax subdivide
    (syntax-rules (event)
      
      ((_) (lambda (context) context)) ;; Base case
      
      ((_ [event pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pcreate context pdur (auto-quasi pdef))])
	   (contexts-merge context ((subdivide rest ...) c)))))

      ((_ [event pdef] rest ...)
       (subdivide [event 1 pdef] rest ...))

      ((_ [key pdur pdef] rest ...)
       (lambda (context)
	 (let ([c (pmorph context pdur (auto-quasi pdef) key)])
	   ((subdivide rest ...) c))))

      ((_ [key pdef] rest ...)
       (subdivide [key 1 pdef] rest ...))))
  )
