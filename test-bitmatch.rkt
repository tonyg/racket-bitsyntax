#lang racket/base

(require "bitstring.rkt")
(require "bitmatch.rkt")
(require rackunit)

(define (experiment-one v)
  (bit-string-case v
    ([(= 0 :bytes 2)] 'yeah)
    ([(? f :bits 10)
      (:discard :bits 6)]
     f)
    ([(? f :bits 10)
      (:discard :bits 6)
      (? rest :binary)]
     (list f rest))))

(check-equal? (experiment-one (bytes 0 0)) 'yeah)
(check-equal? (experiment-one (bytes 252 0)) 1008)
(check-equal? (experiment-one (bytes 252 0 123))
	      (list 1008 (sub-bit-string (bytes 252 0 123) 16 24)))
(check-equal? (bit-string-pack (cadr (experiment-one (bytes 252 0 123))))
	      (bytes 123))
