
(import (chezscheme)
        (testing)
        (harmony)))

(test-eqv "chord-offset"
  C (chord-offset 0 I I major))
(test-eqv "chord-offset"
  E (chord-offset 0 I III major))
(test-eqv "chord-offset"
  G (chord-offset 0 I V major))
(test-eqv "chord-offset"
  D (chord-offset 0 I II major))

(test-eqv "chord-offset"
  17 (chord-offset 1 IV I major))
(test-eqv "chord-offset"
  -10 (chord-offset -2 IX I major))

(define (test-chord name midi-root scale-root scale chord-shape midi-note-list)
  (define result
    (map (lambda (c) (+ midi-root (chord-offset 0 scale-root c scale)))
         chord-shape))
  (define n (string-append "chord: " name))
  (test-list n eqv? midi-note-list result))

(define (make-chord . note-list)
  (map (lambda (x) (+ x 60)) note-list))

;; Test some triads in C major
(test-chord "C major" 60 I major triad-raw 
            (make-chord C E G))
(test-chord "D minor" 60 II major triad-raw 
            (make-chord D F A))
(test-chord "E minor" 60 III major triad-raw 
            (make-chord E G B))
(test-chord "A minor" 60 VI major triad-raw 
            (make-chord A (+ 12 C) (+ 12 E)))
(test-chord "B diminished" 60 VII major triad-raw 
            (make-chord B (+ 12 D) (+ 12 F)))

;; Test some 7ths in C major
(test-chord "C major 7th" 60 I major 7th-raw 
            (make-chord C E G B))
(test-chord "D minor 7th" 60 II major 7th-raw
            (make-chord D F A (+ 12 C)))
(test-chord "B minor 7th flat 5" 60 VII major 7th-raw
            (make-chord B (+ 12 D) (+ 12 F) (+ 12 A)))

;; Test some triads in Ab minor
(test-chord "Ab minor" 68 I minor triad-raw 
            (make-chord Ab B (+ 12 Eb)))
(test-chord "Bb diminshed" 68 II minor triad-raw 
            (make-chord Bb (+ 12 Db) (+ 12 E)))
(test-chord "Eb minor" 68 V minor triad-raw 
            (make-chord Eb Gb Bb))
