#lang racket
(require racket/tcp)

(displayln "portscan (C) 2017 KTG")

(define (tcp-open? host port)
  (with-handlers ([exn:fail:network? (lambda (e) #f)])
    (define-values (inp outp) (tcp-connect host port))
    (close-input-port inp)
    (close-output-port outp)
    #t))

(define (readline prompt)
  (display prompt)
  (read-line (current-input-port) 'any))

(define host (readline "Host: "))
(define port-low (string->number (readline "Port(low): ")))
(define port-high (string->number (readline "Port(high): ")))

(define ports-open '())
(for-each thread-wait
          (for/list ([p (in-range port-low (add1 port-high))])
            (thread (lambda ()
                      (when (tcp-open? host p)
                        (displayln p)
                        (set! ports-open (cons p ports-open)))))))

(printf "Ports open: ~a\n" (sort ports-open <))
