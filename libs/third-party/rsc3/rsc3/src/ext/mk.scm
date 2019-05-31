(mk-r6rs '(rsc3)
	 (cons "../src/sys.guile.scm" rsc3-src)
	 (string-append (list-ref (command-line) 1) "/rsc3.guile.sls")
	 rsc3-dep
	 '()
	 '())

(mk-r6rs '(rsc3)
	 (cons "../src/sys.plt.scm" rsc3-src)
	 (string-append (list-ref (command-line) 1) "/rsc3.mzscheme.sls")
	 (cons '(prefix (scheme) plt:) rsc3-dep)
	 '()
	 '())

