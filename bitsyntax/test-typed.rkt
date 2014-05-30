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

(define-syntax t:named-bit
  (syntax-rules ()
    ((_ #t input ks kf name0 name1)
     (bit-string-case input
       ([ (v :: bits 1) (rest :: binary) ]
	(ks (if (zero? v) name0 name1) rest))
       (else (kf))))
    ((_ #f v name0 name1)
     (cond
      ((eq? v name1) (bit-string (1 :: bits 1)))
      ((eq? v name0) (bit-string (0 :: bits 1)))
      (else (error 't:named-bit
		   "Value supplied is neither ~v nor ~v: ~v"
		   name0 name1 v))))))

(check-equal? (bit-string-case (bytes #xff)
		([ (v :: (t:named-bit 'zero 'one)) (:: bits 7) ]
		 v))
	      'one)

(check-equal? (bit-string->bytes (bit-string ('one :: (t:named-bit 'zero 'one))))
	      (bytes 128))
