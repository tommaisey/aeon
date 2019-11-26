;; This mode is implemented as a derivation of `scheme' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory
;; search facilities are courtesy `find-lisp'.

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)

(defun line-at-point ()
  "Return the line at point as a string."
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (buffer-substring-no-properties beg end)))

;; Scheme.

(defvar rsc3-buffer
  "*rsc3*"
  "*The name of the rsc3 scheme process buffer.")

(defvar rsc3-interpreter
  (list "ikarus")
  "*The name of the scheme interpreter to run (default=\"ikarus\").")

(defvar rsc3-help-directory
  nil
  "*The directory containing the help files (default=nil).")

(defvar rsc3-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(make-variable-buffer-local 'rsc3-literate-p)

(defun rsc3-unlit (s)
  "Remove bird literate marks"
  (if rsc3-literate-p
      (replace-regexp-in-string "^> " "" s)
    s))

(defun rsc3-uncomment (s)
  "Remove initial comment and Bird-literate markers if present"
   (replace-regexp-in-string "^[; ]*>*" "" s))

(defun rsc3-insert-lambda () (interactive) (insert "lambda"))
(defun rsc3-insert-lambda* () (interactive) (insert "λ"))

(defun rsc3-chunk-string (n s)
  "Split a string into chunks of 'n' characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (rsc3-chunk-string n (substring s n))))))

(defun rsc3-send-string (s)
  (if (comint-check-proc rsc3-buffer)
      (let ((cs (rsc3-chunk-string 64 (concat s "\n"))))
        (mapcar
         (lambda (c) (comint-send-string rsc3-buffer c))
         cs))
    (error "no rsc3 process running?")))

(defun rsc3-load-buffer ()
  "Load the current buffer."
  (interactive)
  (save-buffer)
  (rsc3-see-scheme)
  (rsc3-send-string (format "(load \"%s\")" buffer-file-name)))

(defun rsc3-see-scheme ()
  "Arrange so that the frame has two windows, the current buffer is
placed in the upper window and the `rsc3-buffer' in the lower window."
  (interactive)
  (if (not (comint-check-proc rsc3-buffer))
      (rsc3-start-scheme)
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer rsc3-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun rsc3-start-scheme ()
  "Start the rsc3 scheme process.

If `rsc3-interpreter' is not already running as a subprocess it is
started and a new window is created to display the results of
evaluating rsc3 expressions.  Input and output is via `rsc3-buffer'."
  (interactive)
  (if (comint-check-proc rsc3-buffer)
      (rsc3-see-scheme)
    (apply
     'make-comint
     "rsc3"
     (car rsc3-interpreter)
     nil
     (cdr rsc3-interpreter))
    (rsc3-see-scheme)))

(defun rsc3-interrupt-scheme ()
  "Interupt scheme process."
  (interactive)
  (interrupt-process rsc3-buffer comint-ptyp))

(defun rsc3-stop ()
  "Interrup scheme interpreter & reset scsynth"
  (interactive)
  (progn
    (rsc3-interrupt-scheme)
    (sleep-for 0 100)
    (rsc3-reset-scsynth)))

(defun rsc3-quit-scheme ()
  "Quit scheme.

Quit the scheme interpreter and delete the associated buffer."
  (interactive)
  (rsc3-stop)
  (rsc3-evaluate-expression "(exit)")
  (sleep-for 0 100)
  (kill-buffer rsc3-buffer)
  (delete-other-windows))


;; Evaluate.

;; Collects the string containing the text from point back to the
;; start of the preceding expression.

(defun rsc3-expression-before-point ()
  (rsc3-unlit
   (buffer-substring-no-properties
    (save-excursion (backward-sexp) (point))
    (point))))

;; Send the string `expression' to the inferior rsc3 process for
;; evaluation.  If there is not an active sub-process one is started
;; and the text sent.  The message is terminated with a newline
;; character.

(defun rsc3-evaluate-expression (expression)
  "Evaluate an arbitrary expression."
  (interactive "sString to evaluate: ")
  (if (not (comint-check-proc rsc3-buffer))
      (rsc3-start-scheme))
  (rsc3-send-string expression)
  (rsc3-send-string "\n"))

(defun rsc3-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring (line-beginning-position)
			      (line-end-position)))
	 (s* (if rsc3-literate-p
		 (rsc3-unlit s)
	       (rsc3-uncomment s))))
    (rsc3-send-string s*)))

(defun rsc3-evaluate ()
  "Evaluate the complete s-expression that precedes point."
  (interactive)
  (rsc3-evaluate-expression (rsc3-expression-before-point)))

(defun rsc3-play ()
  "Rewrite and evaluate the s-expression that precedes point."
  (interactive)
  (rsc3-evaluate-expression
   (concat "(audition (out 0 " (rsc3-expression-before-point) "))")))

(defun rsc3-draw ()
  "Draw the UGen graph at point using draw*."
  (interactive)
  (rsc3-evaluate-expression
   (concat "(import (rsc3 dot)) (show-graph " (rsc3-expression-before-point) ")")))


;; scsynth

(defun rsc3-boot-scsynth ()
  "Start the current SCSYNTH server and establish a connection."
  (interactive)
  (rsc3-evaluate-expression "(boot*)"))

(defun rsc3-reset-scsynth ()
  "Free all nodes running at the current SCSYNTH server."
  (interactive)
  (rsc3-evaluate-expression "(with-sc3 reset)"))

(defun rsc3-status-scsynth ()
  "Show status at server."
  (interactive)
  (rsc3-evaluate-expression "(with-sc3 display-server-status)"))

(defun rsc3-quit-scsynth ()
  "Shutdown the current SCSYNTH server."
  (interactive)
  (rsc3-evaluate-expression "(with-sc3 (lambda (fd) (async fd /quit)))"))


;; Help

(defun rsc3-help ()
  "Lookup up the symbol at point in the set of Help files
distributed with rsc3."
  (interactive)
  (mapc (lambda (filename)
	  (find-file-other-window filename))
	(find-lisp-find-files rsc3-help-directory
			      (concat "^"
                                      (thing-at-point 'symbol)
				      "\\.help\\.scm"))))


;; Mode

(defvar rsc3-mode-map nil
  "Keymap for rsc3 mode.")

(defun rsc3-mode-keybindings (map)
  "Install rsc3 keybindings into `map'."
  ;; Scheme
  (define-key map [?\C-c ?\C-l] 'rsc3-load-buffer)
  (define-key map [?\C-c ?<] 'rsc3-load-buffer)
  (define-key map [?\C-c ?>] 'rsc3-see-scheme)
  (define-key map [?\C-c ?\C-c] 'rsc3-run-line)
  (define-key map [?\C-c ?\C-q] 'rsc3-quit-scheme)
  (define-key map "\C-\\" 'rsc3-insert-lambda)
  (define-key map "\M-\\" 'rsc3-insert-lambda*)
  (define-key map "\C-c\C-i" 'rsc3-interrupt-scheme)
  ;; scsynth
  (define-key map "\C-c\C-o" 'rsc3-quit-scsynth)
  (define-key map "\C-c\C-k" 'rsc3-reset-scsynth)
  (define-key map "\C-c\C-s" 'rsc3-stop)
  (define-key map "\C-c\C-p" 'rsc3-status-scsynth)
  (define-key map "\C-c\C-b" 'rsc3-boot-scsynth)
  ;; Expression.
  (define-key map "\C-c\C-e" 'rsc3-evaluate)
  (define-key map "\C-c\C-a" 'rsc3-play)
  (define-key map "\C-c\C-g" 'rsc3-draw)
  ;; Help.
  (define-key map "\C-c\C-h" 'rsc3-help))

(defun rsc3-mode-menu (map)
  "Install rsc3 menu into `map'."

  ;; rsc3
  (define-key map [menu-bar rsc3]
    (cons "Scheme-SuperCollider" (make-sparse-keymap "Scheme-SuperCollider")))

  ;; Help
  (define-key map [menu-bar rsc3 help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar rsc3 help rsc3]
    '("Scheme-SuperCollider help" . rsc3-help))

  ;; Expression
  (define-key map [menu-bar rsc3 expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar rsc3 expression draw]
    '("Draw" . rsc3-draw))
  (define-key map [menu-bar rsc3 expression play]
    '("Play" . rsc3-play))
  (define-key map [menu-bar rsc3 expression evaluate]
    '("Evaluate" . rsc3-evaluate))

  ;; Scsynth
  (define-key map [menu-bar rsc3 scsynth]
    (cons "SCSynth" (make-sparse-keymap "SCSynth")))
  (define-key map [menu-bar rsc3 scsynth quit]
    '("Quit scsynth" . rsc3-quit-scsynth))
  (define-key map [menu-bar rsc3 scsynth status]
    '("Display status" . rsc3-status-scsynth))
  (define-key map [menu-bar rsc3 scsynth reset]
    '("Reset scsynth" . rsc3-reset-scsynth))
  (define-key map [menu-bar rsc3 scsynth start]
    '("Boot scsynth" . rsc3-boot-scsynth))

  ;; Scheme
  (define-key map [menu-bar rsc3 scheme]
    (cons "Scheme" (make-sparse-keymap "Scheme")))
  (define-key map [menu-bar rsc3 scheme quit-scheme]
    '("Quit scheme" . rsc3-quit-scheme))
  (define-key map [menu-bar rsc3 scheme interrupt-scheme]
    '("Interrupt scheme" . rsc3-interrupt-scheme))
  (define-key map [menu-bar rsc3 scheme see-output]
    '("See scheme output" . rsc3-see-output))
  (define-key map [menu-bar rsc3 scheme start-scheme]
    '("Start scheme" . rsc3-start-scheme)))

;; If there is no existing map create one and install the keybindings
;; and menu.
(if rsc3-mode-map
    ()
  (let ((map (make-sparse-keymap "Scheme-SuperCollider")))
    (rsc3-mode-keybindings map)
    (rsc3-mode-menu map)
    (setq rsc3-mode-map map)))

(defun rsc3-font-lock-special-forms ()
  "Rules to font lock special forms."
  (interactive)
  (font-lock-add-keywords
   'rsc3-mode
   (list
    (list (concat "(\\(define[-a-zA-Z/\*]*\\)\\>"
		  "[ \t]*(?"
		  "\\(\\sw+\\)?")
	  '(1 font-lock-keyword-face)
	  '(2 (cond ((match-beginning 1) font-lock-function-name-face)
		    ((match-beginning 3) font-lock-variable-name-face)
		    (t font-lock-warning-face))
	      nil t))
    (cons "\\<[akid]r\\>" font-lock-builtin-face)
    (cons "\\<[A-Z][-\\*_a-zA-Z0-9]*\\>" font-lock-type-face)
    (cons "\\<\\sw+:\\>" font-lock-builtin-face))))



(defvar rsc3-font-lock-settings
  '((scheme-font-lock-keywords
     scheme-font-lock-keywords-1 scheme-font-lock-keywords-2)
    nil nil (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
    (font-lock-mark-block-function . mark-defun)))

;; Set up font locking.  This duplicates what scheme.el does, but
;; set case-fold to nil instead of t.  This is required for the math
;; UGen names, which include Not and Abs.

(defun rsc3-setup-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults rsc3-font-lock-settings)
  (put 'letc 'scheme-indent-function 'scheme-let-indent)
  (rsc3-font-lock-special-forms)
  (setq-default font-lock-keywords-case-fold-search nil))

(define-derived-mode
  rsc3-mode
  scheme-mode
  "Scheme SuperCollider"
  "Major mode for interacting with an inferior rsc3 process."
  (rsc3-setup-font-lock)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.ss$" . rsc3-mode))
(add-to-list 'auto-mode-alist '("\\.scm$" . rsc3-mode))

(define-derived-mode
  literate-rsc3-mode
  rsc3-mode
  "Literate Scheme SuperCollider"
  "Major mode for interacting with an inferior rsc3 process."
  (setq hsc3-literate-p t)
  (rsc3-setup-font-lock)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.lss$" . literate-rsc3-mode))

(add-to-list 'interpreter-mode-alist '("rsc3" . rsc3-mode))

(provide 'rsc3)
