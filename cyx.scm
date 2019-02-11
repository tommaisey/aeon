;; Exposes the 'loop' form. The actual subdivision algorithm is
;; implemented in subdivide.scm. Note that this must be imported
;; in a way that makes it available at macroexpand time.
(library (cyx)
  (export cy+ cy! ~)
  (import (scheme) (context)
	  (for (auto-quasi) expand)
	  (for (subdivide) expand))

  (define-syntax cy!
    (syntax-rules ()

      ((_) (lambda (context) context))

      ((_ key pdef)
       (cy! key 1 pdef))
      
      ((_ key pdur pdef)
       (lambda (context)
	 (pmorph context pdur (pdef-quasi pdef) key)))))
  
  (define-syntax cy+
    (syntax-rules ()

      ((_ pdef)
       (cy+ 1 pdef))
      
      ((_ pdur pdef (key r ...) ...)
       (lambda (context)
	 (let* ([c (pcreate context pdur (pdef-quasi pdef))]
		[c ((cy! key r ...) c)] ...)
	   (contexts-merge context c))))))
  )
