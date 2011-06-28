#lang racket/base

(require "bitstring.rkt")
(require "bitmatch.rkt")
(require "bitstitch.rkt")
(require rackunit)

(define (experiment-one v)
  (bit-string-case v
    ([(= 0 :bytes 2)] 'yeah)
    ([(? f :bits 10)
      (:discard :binary)]
     (when (and (< f 123)
		(>= f 100)))
     'between-100-and-123)
    ([(? f :bits 10)
      (:discard :bits 6)]
     f)
    ([(? f :bits 10)
      (:discard :bits 6)
      (? rest :binary)]
     (list f rest))))

(check-equal? (experiment-one (bytes 0 0)) 'yeah)
(check-equal? (experiment-one (bytes 252 0)) 1008)
(check-equal? (experiment-one (bytes 25 64)) 'between-100-and-123)
(check-equal? (experiment-one (bytes 252 0 123))
	      (list 1008 (sub-bit-string (bytes 252 0 123) 16 24)))
(check-equal? (bit-string-pack (cadr (experiment-one (bytes 252 0 123))))
	      (bytes 123))

(check-equal? (bit-string-pack (bit-string (1008 :bits 10) (0 :bits 6)))
	      (bytes 252 0))

(define (pascal->string/utf-8 bs)
  (bit-string-case bs
    ([(? len)
      (? body :binary :bytes len)]
     (bytes->string/utf-8 (bit-string-pack body)))))

(define (string->pascal/utf-8 str)
  (let ((bs (string->bytes/utf-8 str)))
    (bit-string [(bytes-length bs)]
		[bs :binary])))

(check-equal? (pascal->string/utf-8 #"\010abcdefgh") "abcdefgh")
(check-equal? (bit-string-pack (string->pascal/utf-8 "abcdefgh"))
	      #"\010abcdefgh")

(check-equal? (bit-string (49152 :big-endian)) (bytes 0))
(check-equal? (bit-string (49152 :little-endian)) (bytes 0))
(check-equal? (bit-string (49152 :bits 16 :big-endian)) (bytes 192 0))
(check-equal? (bit-string (49152 :bits 16 :little-endian)) (bytes 0 192))

(check-equal? (bit-string [(inexact->exact 1.0)]) (bytes 1))
(check-equal? (bit-string [1.0 :float]) (bytes 63 240 0 0 0 0 0 0))
(check-equal? (bit-string [1.0 :float :bits 32]) (bytes 63 128 0 0))
(check-equal? (bit-string [1.0 :float :little-endian]) (bytes 0 0 0 0 0 0 240 63))
