(import (socket socket))

(define port 57110)
(define ip "127.0.0.1")
(define family AF_INET)

(define socket-fd (socket:socket family SOCK_STREAM IPPROTO_IP))
(socket:bind socket-fd family ip port)
(socket:listen socket-fd)

(printf "listen on ~a\n" port)

(define client-fd (socket:accept socket-fd))
(socket:write client-fd (string->utf8 "Hello World!"))

(socket:close client-fd)
(socket:close socket-fd)
(socket:cleanup)