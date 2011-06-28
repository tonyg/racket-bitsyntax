#lang racket/base

;; As per the Erlang documentation: "A bitstring is a sequence of zero
;; or more bits, where the number of bits doesn't need to be divisible
;; by 8. If the number of bits is divisible by 8, the bitstring is
;; also a binary."
;;
;; A binary, for Racket, is a (bytes?).

;; A Binary is a (bytes ...).

;; A Bitstring is one of
;; - a Binary, a plain 8-bit-aligned binary object
;; - a (bit-slice Binary Number Number), a bit-aligned sub-binary
;; - a (splice Number Bitstring Bitstring), a join of two bitstrings
;;
;; A Bitstring represents a string of bits. Bits are numbered starting
;; from zero, from least- to most-significant, so in a (bytes?), if
;; the 0th byte is #x80, then the 0th bit of the corresponding
;; bitstring is 0 and the 7th bit is 1.

(require rackunit)

(provide bit-slice?
	 bit-slice-binary
	 bit-slice-low-bit
	 bit-slice-high-bit
	 splice?
	 splice-left
	 splice-right
	 bit-string?
	 bit-string-empty?
	 bit-string-length
	 bit-string-append
	 bit-string-split-at-or-false
	 bit-string-split-at
	 sub-bit-string
	 bit-string-byte-count
	 copy-bits!
	 bit-string-pack!
	 bit-string-pack
	 bit-string->bytes
	 bit-string->bytes/pad)

(struct bit-slice (binary low-bit high-bit)
	#:transparent)

(struct splice (length left right)
	#:transparent)

(define (bit-string? x)
  (or (bytes? x)
      (bit-slice? x)
      (splice? x)))

(define (bit-string-empty? x)
  (zero? (bit-string-length x)))

(define (bit-string-length x)
  (cond
   ((bytes? x) (* 8 (bytes-length x)))
   ((bit-slice? x) (- (bit-slice-high-bit x) (bit-slice-low-bit x)))
   ((splice? x) (splice-length x))))

(check-equal? (bit-string-length (bytes)) 0)
(check-equal? (bit-string-length (bytes 255)) 8)
(check-equal? (bit-string-length (bytes 10)) 8)
(check-equal? (bit-string-length (bit-slice (bytes) 0 0)) 0)
(check-equal? (bit-string-length (bit-slice (bytes 255) 1 4)) 3)
(check-equal? (bit-string-length (bit-slice (bytes 10) 1 4)) 3)

(define (abutting? a b)
  (and (bit-slice? a)
       (bit-slice? b)
       (eq? (bit-slice-binary a)
	    (bit-slice-binary b))
       (= (bit-slice-high-bit a)
	  (bit-slice-low-bit b))))

(define abutting-test-binary (bytes 255))
(check-equal? (abutting? abutting-test-binary abutting-test-binary) #f)
(check-equal? (abutting? (bytes 255) (bytes 255)) #f)
(check-equal? (abutting? (bit-slice abutting-test-binary 1 4)
			 (bit-slice (bytes 255) 4 6)) #f)
(check-equal? (abutting? (bit-slice abutting-test-binary 1 4)
			 (bit-slice abutting-test-binary 4 6)) #t)
(check-equal? (abutting? (bit-slice abutting-test-binary 1 4)
			 (bit-slice abutting-test-binary 5 6)) #f)
(check-equal? (abutting? (bit-slice abutting-test-binary 1 3)
			 (bit-slice abutting-test-binary 4 6)) #f)
(check-equal? (abutting? (bit-slice abutting-test-binary 4 6)
			 (bit-slice abutting-test-binary 1 4)) #f)

(define (bit-string-append a b)
  (if (abutting? a b)
      (if (= (bit-string-length (bit-slice-binary a))
	     (+ (bit-string-length a) (bit-string-length b)))
	  (bit-slice-binary a)
	  (bit-slice (bit-slice-binary a)
		     (bit-slice-low-bit a)
		     (bit-slice-high-bit b)))
      (splice (+ (bit-string-length a) (bit-string-length b)) a b)))

(check-equal? (bit-string-append (bit-slice abutting-test-binary 0 4)
				 (bit-slice abutting-test-binary 4 8))
	      abutting-test-binary)
(check-equal? (bit-string-append (bit-slice abutting-test-binary 1 4)
				 (bit-slice abutting-test-binary 4 6))
	      (bit-slice abutting-test-binary 1 6))
(check-equal? (bit-string-append (bit-slice (bytes 255) 1 4)
				 (bit-slice (bytes 255) 4 6))
	      (splice 5
		      (bit-slice (bytes 255) 1 4)
		      (bit-slice (bytes 255) 4 6)))

(define (bit-string-split-at-or-false x offset)
  (let ((len (bit-string-length x)))
    (if (or (negative? offset)
	    (> offset length))
	(values #f #f)
	(let split ((x x)
		    (offset offset))
	  (cond
	   ((bytes? x)
	    (values (bit-slice x 0 offset)
		    (bit-slice x offset (* 8 (bytes-length x)))))
	   ((bit-slice? x)
	    (let ((bin (bit-slice-binary x))
		  (low (bit-slice-low-bit x))
		  (high (bit-slice-high-bit x)))
	      (let ((mid (+ low offset)))
		(values (bit-slice bin low mid)
			(bit-slice bin mid high)))))
	   ((splice? x)
	    (let* ((original-left (splice-left x))
		   (splice-midpoint (bit-string-length original-left)))
	      (cond
	       ((< offset splice-midpoint)
		(let-values (((left mid) (split original-left offset)))
		  (values left
			  (splice (- (splice-length x) offset) mid (splice-right x)))))
	       ((= offset splice-midpoint)
		(values original-left (splice-right x)))
	       (else
		(let-values (((mid right) (split (splice-right x) (- offset splice-midpoint))))
		  (values (splice offset original-left mid)
			  right)))))))))))

(define (bit-string-split-at x offset)
  (let-values (((lhs rhs) (bit-string-split-at-or-false x offset)))
    (if (not lhs)
	(error 'bit-string-split-at "Split point negative or beyond length of string: ~v" offset)
	(values lhs rhs))))

(define (sub-bit-string x low-bit high-bit)
  (when (negative? low-bit)
    (error 'sub-bit-string "Low bit must be non-negative: ~v" low-bit))
  (when (> high-bit (bit-string-length x))
    (error 'sub-bit-string "High bit must be less than or equal to bit string length: ~v" high-bit))
  (cond
   ((bytes? x)
    (bit-slice x low-bit high-bit))
   ((bit-slice? x)
    (let ((old-low (bit-slice-low-bit x)))
      (bit-slice (bit-slice-binary x)
		 (+ old-low low-bit)
		 (+ old-low high-bit))))
   ((splice? x)
    (let-values (((left mid) (bit-string-split-at x low-bit)))
      (let-values (((mid right) (bit-string-split-at mid (- high-bit low-bit))))
	mid)))))

(define (bit-string-byte-count x)
  (quotient (bit-string-length x) 8))

(define (bit-string-byte-count/slop x)
  (quotient/remainder (bit-string-length x) 8))

(define (copy-bits! target target-offset source source-offset remaining-count)
  (let-values (((target-byte target-shift) (quotient/remainder target-offset 8))
	       ((source-byte source-shift) (quotient/remainder source-offset 8)))
    (define (bump-target! bits)
      (let-values (((whole-bytes remaining-bits) (quotient/remainder (+ target-shift bits) 8)))
	(set! target-byte (+ target-byte whole-bytes))
	(set! target-shift remaining-bits)))
    (define (bump-source! bits)
      (let-values (((whole-bytes remaining-bits) (quotient/remainder (+ source-shift bits) 8)))
	(set! source-byte (+ source-byte whole-bytes))
	(set! source-shift remaining-bits)))
    (define (shuffle! bit-count)
      (when (positive? bit-count)
	(let ((old (bytes-ref target target-byte))
	      (new (bytes-ref source source-byte))
	      (mask (sub1 (arithmetic-shift 1 bit-count))))
	  (let ((target-mask (bitwise-not (arithmetic-shift mask target-shift)))
		(source-mask (arithmetic-shift mask source-shift)))
	    (bytes-set! target target-byte
			(bitwise-ior (bitwise-and old target-mask)
				     (arithmetic-shift (bitwise-and new source-mask)
						       (- target-shift source-shift))))))
	(set! remaining-count (- remaining-count bit-count))
	(bump-target! bit-count)
	(bump-source! bit-count)))
    ;; First, align the source to a byte boundary.
    (when (positive? source-shift)
      (let ((source-left-overlap (- 8 source-shift))
	    (remaining-in-first-target-byte (- 8 target-shift)))
	(if (> source-left-overlap remaining-in-first-target-byte)
	    (begin (shuffle! remaining-in-first-target-byte)
		   (shuffle! (- source-left-overlap remaining-in-first-target-byte)))
	    (shuffle! source-left-overlap))))
    ;; At this point the source is aligned to a byte boundary. Copy
    ;; zero or more whole bytes out of source. If the target is
    ;; byte-aligned too, use the builtin bytes-copy!.
    (let ((remaining-whole-bytes (quotient remaining-count 8)))
      (if (zero? target-shift)
	  (begin (bytes-copy! target target-byte
			      source source-byte
			      (+ source-byte remaining-whole-bytes))
		 (set! target-byte (+ target-byte remaining-whole-bytes))
		 (set! source-byte (+ source-byte remaining-whole-bytes))
		 (set! remaining-count (remainder remaining-count 8)))
	  (do ((i 0 (+ i 1)))
	      ((= i remaining-whole-bytes))
	    (shuffle! (- 8 target-shift))
	    (shuffle! (- 8 source-shift)))))
    ;; Now we have fewer than eight bits left to transfer.
    (when (positive? remaining-count)
      (let ((remaining-in-first-target-byte (- 8 target-shift)))
	(if (> remaining-count remaining-in-first-target-byte)
	    (begin (shuffle! remaining-in-first-target-byte)
		   (shuffle! remaining-count))
	    (shuffle! remaining-count))))
    ;; We're finally done.
    ))

(define (bit-string-pack! x buf offset)
  (cond
   ((bytes? x)
    (copy-bits! buf offset x 0 (* 8 (bytes-length x))))
   ((bit-slice? x)
    (copy-bits! buf offset
		(bit-slice-binary x) (bit-slice-low-bit x)
		(- (bit-slice-high-bit x) (bit-slice-low-bit x))))
   ((splice? x)
    (let* ((left (splice-left x))
	   (left-length (bit-string-length left)))
      (bit-string-pack! left buf offset)
      (bit-string-pack! (splice-right x) buf (+ offset left-length))))))

(define (flatten-to-bytes x min-width)
  (let-values (((byte-count bits-remaining)
		(bit-string-byte-count/slop x)))
    (let ((buf (make-bytes (max byte-count min-width) 0)))
      (bit-string-pack! x buf 0)
      (if (zero? bits-remaining)
	  buf
	  (bit-slice buf 0 (bit-string-length x))))))

(define (bit-string-pack x)
  (cond
   ((bytes? x)
    x)
   ((bit-slice? x)
    (if (= (bytes-length (bit-slice-binary x))
	   (bit-string-byte-count x))
	x
	(flatten-to-bytes x 0)))
   ((splice? x)
    (flatten-to-bytes x 0))))

(define (bit-string->bytes x)
  (bit-string->bytes/pad x 0))

(define (bit-string->bytes/pad x min-width)
  (if (bytes? x)
      x
      (let ((v (flatten-to-bytes x min-width)))
	(if (bit-slice? v)
	    (bit-slice-binary v)
	    v))))
