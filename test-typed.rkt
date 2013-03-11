#lang typed/racket

(require typed/rackunit)

(require "main.rkt")

(check-equal? (bit-string->bytes (bit-string-case (bytes 4 65 66 67 68)
				   ([len (body :: binary bytes len)]
				    body)
				   ([(rest :: binary)]
				    rest)))
	      #"ABCD")

(check-equal? (bit-string-case (bytes 10 65 66 67 68 69 70 71 72 73 74)
		([len (val :: integer bytes len)]
		 val))
	      #x4142434445464748494a)

(check-equal? (bit-string-case (bytes 4 65 66 67 68 2 3 4)
		([len (body :: binary bytes len) v1 (v2 :: bits 16)]
		 (list (bit-string->bytes body) v1 v2)))
	      (list #"ABCD" 2 772))
