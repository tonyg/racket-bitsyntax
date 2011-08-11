#lang racket/base

(require "bitstring.rkt")
(require "bitmatch.rkt")
(require "bitstitch.rkt")
(require rackunit)

(define (experiment-one v)
  (bit-string-case v
    ([(= 0 :: bytes 2)] 'yeah)
    ([(f :: bits 10) (:: binary)]
     (when (and (< f 123)
		(>= f 100)))
     'between-100-and-123)
    ([(f :: bits 10) (:: bits 6)]
     f)
    ([(f :: bits 10) (:: bits 6) (rest :: binary)]
     (list f rest))))

(check-equal? (experiment-one (bytes 0 0)) 'yeah)
(check-equal? (experiment-one (bytes 252 0)) 1008)
(check-equal? (experiment-one (bytes 25 64)) 'between-100-and-123)
(check-equal? (experiment-one (bytes 252 0 123))
	      (list 1008 (sub-bit-string (bytes 252 0 123) 16 24)))
(check-equal? (bit-string-pack (cadr (experiment-one (bytes 252 0 123))))
	      (bytes 123))

(check-equal? (bit-string-pack (bit-string (1008 :: bits 10) (0 :: bits 6)))
	      (bytes 252 0))

(define (pascal->string/utf-8 bs)
  (bit-string-case bs
    ([len (body :: binary bytes len)]
     (bytes->string/utf-8 (bit-string-pack body)))))

(define (string->pascal/utf-8 str)
  (let ((bs (string->bytes/utf-8 str)))
    (bit-string (bytes-length bs) (bs :: binary))))

(check-equal? (pascal->string/utf-8 #"\010abcdefgh") "abcdefgh")
(check-equal? (bit-string-pack (string->pascal/utf-8 "abcdefgh"))
	      #"\010abcdefgh")

(check-equal? (bit-string (49152 :: big-endian)) (bytes 0))
(check-equal? (bit-string (49152 :: little-endian)) (bytes 0))
(check-equal? (bit-string (49152 :: bits 16 big-endian)) (bytes 192 0))
(check-equal? (bit-string (49152 :: bits 16 little-endian)) (bytes 0 192))

(check-equal? (bit-string (inexact->exact 1.0)) (bytes 1))
(check-equal? (bit-string [1.0 :: float]) (bytes 63 240 0 0 0 0 0 0))
(check-equal? (bit-string [1.0 :: float bits 32]) (bytes 63 128 0 0))
(check-equal? (bit-string [1.0 :: float little-endian]) (bytes 0 0 0 0 0 0 240 63))

(define (p:pascal-string/utf-8 is-matching)
  (if is-matching
      (lambda (input ks kf)
	(bit-string-case input
	  ([len (body :: binary bytes len) (rest :: binary)]
	   (ks (bytes->string/utf-8 (bit-string->bytes body)) rest))
	  (else
	   (kf))))
      (lambda (str)
	(let* ((bs (string->bytes/utf-8 str))
	       (len (bytes-length bs)))
	  (when (> len 255)
	    (error 'p:pascal-string
		   "String of length ~v too long; max is 255 encoded bytes"
		   len))
	  (bit-string len
		      (bs :: binary bytes len))))))

(define (p:d bs)
  (bit-string-case bs
    ([ (s :: (p:pascal-string/utf-8)) (rest :: binary) ]
     (list s (bit-string-pack rest)))
    (else #f)))

(define (p:e str)
  (bit-string-pack (bit-string (str :: (p:pascal-string/utf-8)))))

(check-equal? (p:d #"\010abcdefghijkl") (list "abcdefgh" #"ijkl"))
(check-equal? (p:d #"\010abcdefgh") (list "abcdefgh" #""))
(check-equal? (p:d #"\010abcd") #f)
(check-equal? (p:d #"\000") (list "" #""))

(check-equal? (p:e "abcdefgh") #"\010abcdefgh")
(check-equal? (p:e "") #"\000")
(check-equal? (p:e (make-string 255 #\a))
	      (bytes-append (bytes 255)
			    (make-bytes 255 (char->integer #\a))))
(check-exn #rx"too long"
	   (lambda () (p:e (make-string 256 #\a))))

(define-syntax m:test
  (syntax-rules ()
    ((_ #t substval)
     (lambda (input ks kf)
       (ks substval input)))
    ((_ #f substval)
     (lambda (dontcare)
       (bytes substval)))))

(check-equal? (bit-string-case #"" ([ (v :: (m:test 123)) ] v)) 123)
(check-equal? (bit-string (234 :: (m:test 123))) (bytes 123))

(define-syntax utf-8
  (syntax-rules ()
    [(_ #t)
     (lambda (input ks kf)
       (ks (bytes->string/utf-8 (bit-string->bytes input)) (bytes)))]
    [(_ #t length-in-bytes)
     (lambda (input ks kf)
       (bit-string-case input
	 ([ (body :: binary bytes length-in-bytes)
	    (rest :: binary) ]
	  (ks (bytes->string/utf-8 (bit-string->bytes body)) rest))
	 (else
	  (kf))))]
    [(_ #f)
     (lambda (str)
       (string->bytes/utf-8 str))]
    [(_ #f (length-format-options ...))
     (lambda (str)
       (let* ((bs (string->bytes/utf-8 str))
	      (len (bytes-length bs)))
	 (bit-string (len :: length-format-options ...)
		     (bs :: binary))))]))

(check-equal? (bit-string-case (bytes #xc3 #xa5 #xc3 #xa4 #xc3 #xb6)
		([ (s :: (utf-8)) ] s))
	      (list->string (map integer->char (list 229 228 246))))

(check-equal? (bit-string-case (bytes #xc3 #xa5 #xc3 #xa4 #xc3 #xb6)
		([ (s :: (utf-8 4)) (rest :: binary) ] (list s (bit-string->bytes rest))))
	      (list (list->string (map integer->char (list 229 228)))
		    (bytes #xc3 #xb6)))

(check-equal? (bit-string->bytes
	       (bit-string ((list->string (map integer->char (list 229 228 246)))
			    :: (utf-8))))
	      (bytes #xc3 #xa5 #xc3 #xa4 #xc3 #xb6))

(check-equal? (bit-string->bytes
	       (bit-string ((list->string (map integer->char (list 229 228 246)))
			    :: (utf-8 (integer bytes 4)))))
	      (bytes #x00 #x00 #x00 #x06 #xc3 #xa5 #xc3 #xa4 #xc3 #xb6))
