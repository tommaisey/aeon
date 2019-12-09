(module harmony-tests ()

  (import (chezscheme)
          (testing)
          (harmony)) 

  ;; A couple of helper fns
  (define (test-chord name midi-root scale-root scale chord-shape midi-note-list)
    (define result
      (map (lambda (c) (+ midi-root (chord-offset 0 scale-root c scale)))
           (vector->list chord-shape)))
    (test-list (string-append "chord: " name) eqv? midi-note-list result))

  (define (make-chord . chord-notes)
    (map (lambda (x) (+ x)) chord-notes))

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
              (list C E G))
  (test-chord "D minor" 60 II major triad
              (list D F A))
  (test-chord "E minor" 60 III major triad
              (list E G B))
  (test-chord "A minor" 60 VI major triad
              (list A C4 E4))
  (test-chord "B diminished" 60 VII major triad
              (list B D4 F4))

  ;; Test some 7ths in C major
  (test-chord "C major 7th" 60 I major 7th
              (list C E G B))
  (test-chord "D minor 7th" 60 II major 7th
              (list D F A C4))
  (test-chord "B minor 7th flat 5" 60 VII major 7th
              (list B D4 F4 A4))

  ;; Test some triads in Ab minor
  (test-chord "Ab minor" 68 I minor triad
              (list Ab B Eb4))
  (test-chord "Bb diminshed" 68 II minor triad
              (list Bb Db4 E4))
  (test-chord "Eb minor" 68 V minor triad
              (list Eb4 Gb4 Bb4))

  ;; Test some 7ths in F# minor
  (test-chord "F# minor 7th" 66 I minor 7th
              (list Fs A Cs4 E4))
  (test-chord "G# minor seventh flat 5" 66 II minor 7th
              (list Gs B D4 Fs4))

  )