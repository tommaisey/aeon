(import (socket socket))

(define port 57110)
(define ip "127.0.0.1")
(define family AF_INET)

(define socket-fd (socket:socket family SOCK_STREAM IPPROTO_IP))
(socket:connect socket-fd family ip port)

(socket:write socket-fd (string->utf8 "Hello Server!\n"))

(define buff (socket:read socket-fd))
(printf "client: ~a\n" (utf8->string buff))

(socket:close socket-fd)
(socket:cleanup)
