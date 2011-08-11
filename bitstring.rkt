#lang racket/base

;; As per the Erlang documentation: "A bitstring is a sequence of zero
;; or more bits, where the number of bits doesn't need to be divisible
;; by 8. If the number of bits is divisible by 8, the bitstring is
;; also a binary."
;;
;; They don't make it clear in the documentation that I can see, but
;;     <<_:6, F:10, _:8>> = <<255, 240, 0>>.
;; gives a binding of F = 1008, rather than 963, so clearly when
;; destructuring a binary bits are counted off from the MSB of a byte
;; to the LSB rather than the other way around!
;;
;; A binary, for Racket, is a (bytes?).

;; A Binary is a (bytes ...).

;; A Bitstring is one of
;; - a Binary, a plain 8-bit-aligned binary object
;; - a (bit-slice Binary Number Number), a bit-aligned sub-binary
;; - a (splice Number Bitstring Bitstring), a join of two bitstrings
;;
;; A Bitstring represents a string of bits, numbered ascending from
;; zero. Extraction of bytes from a bit string is as follows:
;; - the first byte is bits 0 through 7 inclusive; bit 0 is the MOST
;;   significant bit in the byte.
;; - the second byte is bits 8 through 15;
;; - etc.
;;
;; All integer quantities 8 bits or shorter that are read out of a bit
;; string are read in this way. Their most-significant bit is the
;; lowest-numbered bit in the bit string. It is only when numbers
;; larger than 8 bits are read out that endianness comes into
;; play. Whether big- or little-endian interpretations are used, the
;; most-significant bit in any byte-size piece, no matter its
;; alignment, is always the lowest-numbered bit in the bit string.
;;
;; This is not quite what one would expect from a mathematical point
;; of view (and it doesn't line up too closely with Racket's bit
;; manipulation primitives either) but makes sense when considering
;; the way network packets are written down and thought about, and is
;; compatible with Erlang to boot.

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
	 bit-string-ref
	 sub-bit-string
	 bit-string-byte-count
	 copy-bits!
	 bit-string-pack!
	 bit-string-pack
	 bit-string->bytes
	 bit-string->bytes/align
	 bit-string->integer
	 integer->bit-string)

(struct bit-slice (binary low-bit high-bit)
	#:transparent)

(struct splice (length left right)
	#:transparent)

(define (bit-string? x)
  (or (bytes? x)
      (bit-slice? x)
      (splice? x)))

(check-equal? (bit-string? "hello") #f)
(check-equal? (bit-string? 123) #f)
(check-equal? (bit-string? #"hello") #t)
(check-equal? (bit-string? (bit-slice #"hello" 2 4)) #t)
(check-equal? (bit-string? (splice 0 (bytes) (bytes))) #t)

(define (bit-string-empty? x)
  (zero? (bit-string-length x)))

(define (bit-string-length x)
  (cond
   ((bytes? x) (* 8 (bytes-length x)))
   ((bit-slice? x) (- (bit-slice-high-bit x) (bit-slice-low-bit x)))
   ((splice? x) (splice-length x))))

(check-equal? (bit-string-empty? (bytes)) #t)
(check-equal? (bit-string-empty? (bytes 1)) #f)

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

(define (bit-string-append-2 a b)
  (if (abutting? a b)
      (if (= (bit-string-length (bit-slice-binary a))
	     (+ (bit-string-length a) (bit-string-length b)))
	  (bit-slice-binary a)
	  (bit-slice (bit-slice-binary a)
		     (bit-slice-low-bit a)
		     (bit-slice-high-bit b)))
      (splice (+ (bit-string-length a) (bit-string-length b)) a b)))

(define bit-string-append
  (case-lambda
   [(a b) (bit-string-append-2 a b)]
   [(a . rest)
    (foldl (lambda (rhs lhs) (bit-string-append-2 lhs rhs))
	   a
	   rest)]))

(check-equal? (bit-string-append abutting-test-binary)
	      abutting-test-binary)
(check-equal? (bit-string-append (bit-slice abutting-test-binary 0 4)
				 (bit-slice abutting-test-binary 4 8))
	      abutting-test-binary)
(check-equal? (bit-string-append (bit-slice abutting-test-binary 0 4)
				 (bit-slice abutting-test-binary 4 6)
				 (bit-slice abutting-test-binary 6 8))
	      abutting-test-binary)
(check-equal? (bit-string-append (bit-slice abutting-test-binary 1 4)
				 (bit-slice abutting-test-binary 4 6))
	      (bit-slice abutting-test-binary 1 6))
(check-equal? (bit-string-append (bit-slice (bytes 255) 1 4)
				 (bit-slice (bytes 255) 4 6))
	      (splice 5
		      (bit-slice (bytes 255) 1 4)
		      (bit-slice (bytes 255) 4 6)))

(check-equal? (bit-string-length (bit-string-append (bytes 1) (bytes 2))) 16)

(define (bit-string-split-at-or-false x offset)
  (let ((len (bit-string-length x)))
    (if (or (negative? offset)
	    (> offset len))
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

(define (bit-string-ref x offset)
  (when (negative? offset)
    (error 'bit-string-ref "Offset must be non-negative: ~v" offset))
  (when (>= offset (bit-string-length x))
    (error 'bit-string-ref "Offset must be less than or equal to bit string length: ~v" offset))
  (let search ((x x)
	       (offset offset))
    (cond
     ((bytes? x)
      (let-values (((byte-offset bit-offset) (quotient/remainder offset 8)))
	(bitwise-bit-field (bytes-ref x byte-offset) (- 7 bit-offset) (- 8 bit-offset))))
     ((bit-slice? x)
      (bit-string-ref (bit-slice-binary x) (+ (bit-slice-low-bit x) offset)))
     ((splice? x)
      (let* ((left (splice-left x))
	     (midpoint (bit-string-length left)))
	(if (< offset midpoint)
	    (search left offset)
	    (search (splice-right x) (- offset midpoint))))))))

(check-equal? (bit-string-ref (bytes #x80) 0) 1)
(check-equal? (bit-string-ref (bytes #x80) 7) 0)
(check-equal? (bit-string-ref (bytes #x20) 2) 1)
(check-equal? (bit-string-ref (bytes #x01) 0) 0)
(check-equal? (bit-string-ref (bytes #x01) 7) 1)
(check-equal? (bit-string-ref (bytes #x00 #x80) 8) 1)
(check-equal? (bit-string-ref (bytes #x00 #x01) 15) 1)
(check-equal? (bit-string-ref (bit-slice (bytes #x20) 2 3) 0) 1)
(check-equal? (bit-string-ref (bit-slice (bytes #x40) 2 3) 0) 0)

(define (sub-bit-string x low-bit high-bit)
  (when (negative? low-bit)
    (error 'sub-bit-string "Low bit must be non-negative: ~v" low-bit))
  (when (> high-bit (bit-string-length x))
    (error 'sub-bit-string
	   "High bit must be less than or equal to bit string length: ~v" high-bit))
  (cond
   ((bytes? x)
    (if (and (zero? low-bit)
	     (= high-bit (* 8 (bytes-length x))))
	x
	(bit-slice x low-bit high-bit)))
   ((bit-slice? x)
    (let ((old-low (bit-slice-low-bit x)))
      (bit-slice (bit-slice-binary x)
		 (+ old-low low-bit)
		 (+ old-low high-bit))))
   ((splice? x)
    (let-values (((left mid) (bit-string-split-at x low-bit)))
      (let-values (((mid right) (bit-string-split-at mid (- high-bit low-bit))))
	mid)))))

(define (bits->bytes bit-count)
  (quotient (+ 7 bit-count) 8))

(define (bit-string-byte-count x)
  (bits->bytes (bit-string-length x)))

(check-equal? (bit-string-byte-count (bytes #xff)) 1)
(check-equal? (bit-string-byte-count (bytes #xff #x00)) 2)
(check-equal? (bit-string-byte-count (bit-slice (bytes #xff #x00) 6 16)) 2)
(check-equal? (bit-string-byte-count (bit-slice (bytes #xff #x00) 6 14)) 1)

(define (bits->bytes+slop bit-count)
  (let* ((byte-count (quotient (+ 7 bit-count) 8))
	 (slop (- (* 8 byte-count) bit-count)))
    (values byte-count slop)))

(define (bit-string-byte-count+slop x)
  (bits->bytes+slop (bit-string-length x)))

(check-equal? (let-values (((b s) (bit-string-byte-count+slop
				   (bit-slice (bytes #xff #x00) 6 16))))
		(list b s))
	      (list 2 6))

(define (bit-mask width)
  (sub1 (arithmetic-shift 1 width)))

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
	      (mask (bit-mask bit-count)))
	  (let ((target-mask (bitwise-not (arithmetic-shift mask (- 8 target-shift bit-count))))
		(source-mask (arithmetic-shift mask (- 8 source-shift bit-count))))
	    (bytes-set! target target-byte
			(bitwise-ior (bitwise-and old target-mask)
				     (arithmetic-shift (bitwise-and new source-mask)
						       (- source-shift target-shift))))))
	(set! remaining-count (- remaining-count bit-count))
	(bump-target! bit-count)
	(bump-source! bit-count)))
    ;; First, align the source to a byte boundary.
    (when (positive? source-shift)
      (let ((source-left-overlap (min remaining-count (- 8 source-shift)))
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

(check-equal? (let ((buf (bytes 0))) (copy-bits! buf 4 (bytes 255 255 255) 17 4) buf)
	      (bytes 15))
(check-equal? (let ((buf (bytes 0 0 0 0)))
		(copy-bits! buf 6 (bytes 255 255 255 255) 2 12)
		buf)
	      (bytes #b00000011 #b11111111 #b11000000 #b00000000))

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

(check-equal? (let ((buf (bytes 0 0 0 0)))
		(bit-string-pack! (bytes 255) buf 4)
		buf)
	      (bytes 15 240 0 0))

(define (flatten-to-bytes x align-right?)
  (let-values (((byte-count bits-remaining)
		(bit-string-byte-count+slop x)))
    (let ((buf (make-bytes byte-count 0)))
      (bit-string-pack! x buf (if align-right? bits-remaining 0))
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
	(flatten-to-bytes x #f)))
   ((splice? x)
    (flatten-to-bytes x #f))))

(check-equal? (bit-string-pack (bytes 1)) (bytes 1))
(check-equal? (bit-string-pack (bit-slice (bytes 255 255) 2 14))
	      (bit-slice (bytes 255 255) 2 14))
(check-equal? (bit-string-pack (bit-slice (bytes 255 255) 2 4))
	      (bit-slice (bytes 192) 0 2))

(define (bit-string->bytes x)
  (bit-string->bytes/align x #f))

(define (bit-string->bytes/align x align-right?)
  (if (bytes? x)
      x
      (let ((v (flatten-to-bytes x align-right?)))
	(if (bit-slice? v)
	    (bit-slice-binary v)
	    v))))

;; 1111 1111 1111 0000 0000 0000
;;        -- ---- ----
;;
;; packed into bytes, yielding two bytes worth, and per the rules
;; described above, the bits are copied left to right, so
;;
;; 1111 1100 0000 0000
;; ---- ---- --
(check-equal? (bit-string-pack (bit-slice (bytes 255 240 0) 6 16))
	      (bit-slice (bytes 252 0) 0 10))
(check-equal? (bit-string->bytes (bit-slice (bytes 255 240 0) 6 16))
	      (bytes 252 0))

;; Aligned right, that'll be
;; 0000 0011 1111 0000
;;        -- ---- ----
(check-equal? (bit-string->bytes/align (bit-slice (bytes 255 240 0) 6 16) #t)
	      (bytes 3 240))

(define (bit-string->integer x big-endian? signed?)
  (let ((width (bit-string-length x)))
    (define (fix-signed value)
      (cond
       ((not signed?) value)
       ((< value (arithmetic-shift 1 (sub1 width))) value)
       (else (- value (arithmetic-shift 1 width)))))
    (if big-endian?
	(let* ((bs (bit-string->bytes/align x #t))
	       (count (bytes-length bs)))
	  (do ((i 0 (+ i 1))
	       (acc 0 (bitwise-ior (arithmetic-shift acc 8)
				   (bytes-ref bs i))))
	      ((= i count) (fix-signed acc))))
	(let* ((bs (bit-string->bytes/align x #f))
	       (count (bytes-length bs)))
	  (do ((i (- count 1) (- i 1))
	       (acc 0 (bitwise-ior (arithmetic-shift acc 8)
				   (bytes-ref bs i))))
	      ((< i 0) (fix-signed acc)))))))

(check-equal? (bit-string->integer (bytes 1 2 3 4) #t #f) #x01020304)
(check-equal? (bit-string->integer (bytes 129 2 3 4) #t #f) #x81020304)
(check-equal? (bit-string->integer (bytes 129 2 3 4) #t #t) (- #x81020304 #x100000000))
(check-equal? (bit-string->integer (bytes 1 2 3 4) #f #f) #x04030201)
(check-equal? (bit-string->integer (bytes 1 2 3 132) #f #f) #x84030201)
(check-equal? (bit-string->integer (bytes 1 2 3 132) #f #t) (- #x84030201 #x100000000))

(check-equal? (bit-string->integer (bit-slice (bytes 255 240 0) 6 16) #f #f) 252)
(check-equal? (bit-string->integer (bit-slice (bytes 255 240 0) 6 16) #f #t) 252)
(check-equal? (bit-string->integer (bit-slice (bytes 255 240 0) 6 16) #t #f) 1008)
(check-equal? (bit-string->integer (bit-slice (bytes 255 240 0) 6 16) #t #t) -16)

;; Signedness doesn't matter here - it only matters for decoding ints
;; from bitstrings.
(define (integer->bit-string n width big-endian?)
  (let-values (((whole-bytes bits-remaining) (bits->bytes+slop width)))
    (let ((bin (make-bytes whole-bytes)))
      (if big-endian?
	  (do ((i 0 (+ i 1)))
	      ((= i whole-bytes)
	       (sub-bit-string bin bits-remaining (* 8 whole-bytes)))
	    (let ((low-bit (* (- whole-bytes i 1) 8)))
	      (bytes-set! bin i (bitwise-bit-field n low-bit (+ low-bit 8)))))
	  (do ((i 0 (+ i 1)))
	      ((= i whole-bytes)
	       (sub-bit-string bin 0 width))
	    (let ((low-bit (* i 8)))
	      (bytes-set! bin i (bitwise-bit-field n low-bit (+ low-bit 8)))))))))

(check-equal? (integer->bit-string #x01020304 32 #t) (bytes 1 2 3 4))
(check-equal? (integer->bit-string #x81020304 32 #t) (bytes 129 2 3 4))
(check-equal? (integer->bit-string (- #x81020304 #x100000000) 32 #t) (bytes 129 2 3 4))
(check-equal? (integer->bit-string #x04030201 32 #f) (bytes 1 2 3 4))
(check-equal? (integer->bit-string #x84030201 32 #f) (bytes 1 2 3 132))
(check-equal? (integer->bit-string (- #x84030201 #x100000000) 32 #f) (bytes 1 2 3 132))

(check-equal? (integer->bit-string 252 10 #f)  (bit-slice (bytes 252 0) 0 10))
(check-equal? (integer->bit-string 1008 10 #t) (bit-slice (bytes 3 240) 6 16))
(check-equal? (integer->bit-string -16 10 #t)  (bit-slice (bytes 255 240) 6 16))
;;                                                               ^^^
;; That this is not 3 is insignificant. The bit-slice says that bits number 0-5 are
;; not part of the answer.

(check-equal? (let-values (((a b) (bit-string-split-at-or-false (bytes 1) -2)))
		(or a b))
	      #f)
(check-equal? (let-values (((a b) (bit-string-split-at-or-false (bytes 1) 2)))
		(and (equal? (bit-string->bytes/align a #t) (bytes 0))
		     (equal? (bit-string->bytes/align b #t) (bytes 1))))
	      #t)
(check-equal? (let-values (((a b) (bit-string-split-at-or-false (bytes 1) 20)))
		(or a b))
	      #f)

(check-equal? (bit-string-ref (bit-string-append (bytes 4) (bytes 0)) 5) 1)
(check-equal? (bit-string-ref (bit-string-append (bytes 0) (bytes 4)) 13) 1)

(check-equal? (bit-string->bytes (sub-bit-string (sub-bit-string (bytes 255) 1 6)
						 1 4))
	      (bytes #xe0))

(check-equal? (bit-string->bytes
	       (sub-bit-string (bit-string-append (bytes 1) (bytes 2)) 0 8))
	      (bytes 1))
(check-equal? (bit-string-pack (sub-bit-string
				(bit-string-append (bytes 1)
						   (bit-slice (bytes 255) 1 6))
				4 12))
	      (bytes 31))
