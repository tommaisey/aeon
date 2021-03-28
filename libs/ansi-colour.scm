#!chezscheme

;; Simple routine to colourise text for terminal output.
(library (ansi-colour)
  (export colourise-text
          :red :green :yellow :blue
          :red-light :green-light :blue-light)

  (import (chezscheme) (utilities))

  (declare-keywords
   :red :green :yellow :blue
   :red-light :green-light :blue-light)

  (define (colourise-text colour-key text)
    (str+ (colour-string colour-key) text end-colour))

  (define (colour-string colour-key)
    (case colour-key
      [:red "\033[0;31m"]
      [:green "\033[0;32m"]
      [:yellow "\033[1;33m"]
      [:blue "\033[1;34m"]
      [:red-light "\033[1;31m"]
      [:green-light "\033[1;32m"]
      [:blue-light "\033[1;34m"]))

  (define end-colour "\033[0m"))
