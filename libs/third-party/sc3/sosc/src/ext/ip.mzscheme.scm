;; data udp
(plt:define-struct udp* (s h p))

;; any -> bool
(define udp:socket?
  udp*?)

;; string -> int -> socket
(define udp:open
  (lambda (h p)
    (make-udp* (plt:udp-open-socket h p) h p)))

;; socket -> bytevector -> ()
(define udp:send
  (lambda (u b)
    (let ((s (udp*-s u))
	  (h (udp*-h u))
	  (p (udp*-p u)))
      (plt:udp-send-to* s h p b))))

;; socket -> maybe bytevector
(define udp:recv
  (lambda (u)
    (let* ((s (udp*-s u))
	   (h (udp*-h u))
	   (p (udp*-p u))
	   (b (plt:make-bytes 8192))
	   (r (plt:sync/timeout 1.0 (plt:udp-receive!-evt s b))))
      (if r
	  (plt:subbytes b 0 (plt:car r))
	  #f))))

;; socket -> ()
(define udp:close
  (lambda (u)
    (plt:udp-close (udp*-s u))))

;; data tcp
(plt:define-struct tcp* (i o h p))

;; any -> bool
(define tcp:socket?
  tcp*?)

;; string -> int -> socket
(define tcp:open
  (lambda (h p)
    (let-values
     (((i o) (plt:tcp-connect h p)))
     (plt:file-stream-buffer-mode i 'none)
     (plt:file-stream-buffer-mode o 'none)
     (make-tcp* i o h p))))

;; socket -> bytevector -> ()
(define tcp:send
  (lambda (fd b)
    (let ((o (tcp*-o fd)))
      (put-bytevector o b))))

;; socket -> int -> maybe bytevector
(define tcp:read
  (lambda (fd n)
    (get-bytevector-n (tcp*-i fd) n)))

;; socket -> ()
(define tcp:close
  (lambda (fd)
    (close-input-port (tcp*-i fd))
    (close-output-port (tcp*-o fd))))
