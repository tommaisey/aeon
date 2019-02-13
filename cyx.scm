(library (cyx)
  (export in+ to! ~)
  (import (scheme) (context)
	  (for (auto-quasi) expand)
	  (for (subdivide) expand))

  ;; A cnode that sets a property of events according to a subdividing pattern.
  (define-syntax to!
    (syntax-rules ()

      ((_ key pdef)
       (to! key 1 pdef))
      
      ((_ key pdur pdef)
       (lambda (context)
	 (to!impl context pdur (pdef-quasi pdef) key)))))

  ;; A cnode that adds blank events according to a subdividing pattern. 
  (define-syntax in+
    (syntax-rules ()

      ((_ pdef)
       (in+ 1 pdef))
      
      ((_ pdur pdef)
       (lambda (context)
	 (contexts-merge context (in+impl context pdur (pdef-quasi pdef)))))))
  )
