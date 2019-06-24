(module harmony-tests ()

  (import (chezscheme)
          (testing)
          (harmony)) 

  ;; A couple of helper fns
  (define (test-chord name midi-root scale-root scale chord-shape midi-note-list)
    (define result
      (map (lambda (c) (+ midi-root (chord-offset 0 scale-root c scale)))
           (vector->list chord-shape)))
    (define n (string-append "chord: " name))
    (test-list n eqv? midi-note-list result))

  (define (make-chord octave . chord-notes)
    (map (lambda (x) (+ x (* 12 (+ octave 2)))) chord-notes))

  ;; Test the chord-offset function
  (test-eqv "chord-offset"
    0 (chord-offset 0 I I major))
  (test-eqv "chord-offset"
    4 (chord-offset 0 I III major))
  (test-eqv "chord-offset"
    7 (chord-offset 0 I V major))
  (test-eqv "chord-offset"
    2 (chord-offset 0 I II major))

  ;; With octaves
  (test-eqv "chord-offset, octaves"
    17 (chord-offset 1 IV I major))
  (test-eqv "chord-offset, octaves"
    -10 (chord-offset -2 IX I major))

  ;; With negative & large scale degrees
  (test-eqv "chord-offset, negative scd"
    -1 (chord-offset 0 -1 I major))
  (test-eqv "chord-offset, negative scd"
    -3 (chord-offset 0 -2 I major))
  (test-eqv "chord-offset, negative scd"
    -12 (chord-offset 0 -7 I major))
  (test-eqv "chord-offset, negative scd"
    -13 (chord-offset 0 -8 I major))
  (test-eqv "chord-offset, negative scd and octave"
    -13 (chord-offset -1 -1 I major))

  (test-eqv "chord-offset, negative scd plus chd"
    2 (chord-offset 0 -2 3 major))
  (test-eqv "chord-offset, negative scd plus chd"
    -8 (chord-offset 0 -8 3 major))

  ;; Test some triads in C major
  (test-chord "C major" 60 I major triad
              (make-chord 3 C E G))
  (test-chord "D minor" 60 II major triad
              (make-chord 3 D F A))
  (test-chord "E minor" 60 III major triad
              (make-chord 3 E G B))
  (test-chord "A minor" 60 VI major triad
              (make-chord 3 A (+ 12 C) (+ 12 E)))
  (test-chord "B diminished" 60 VII major triad
              (make-chord 3 B (+ 12 D) (+ 12 F)))

  ;; Test some 7ths in C major
  (test-chord "C major 7th" 60 I major 7th
              (make-chord 3 C E G B))
  (test-chord "D minor 7th" 60 II major 7th
              (make-chord 3 D F A (+ 12 C)))
  (test-chord "B minor 7th flat 5" 60 VII major 7th
              (make-chord 3 B (+ 12 D) (+ 12 F) (+ 12 A)))

  ;; Test some triads in Ab minor
  (test-chord "Ab minor" 68 I minor triad
              (make-chord 3 Ab B (+ 12 Eb)))
  (test-chord "Bb diminshed" 68 II minor triad
              (make-chord 3 Bb (+ 12 Db) (+ 12 E)))
  (test-chord "Eb minor" 68 V minor triad
              (make-chord 4 Eb Gb Bb))

  ;; Test some 7ths in F# minor
  (test-chord "F# minor 7th" 66 I minor 7th
              (make-chord 3 Fs A (+ 12 Cs) (+ 12 E)))
  (test-chord "G# minor seventh flat 5" 66 II minor 7th
              (make-chord 3 Gs B (+ 12 D) (+ 12 Fs)))

  )

