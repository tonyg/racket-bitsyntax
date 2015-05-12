#lang typed/racket/base

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

;; A BitString is one of
;; - a Binary, a plain 8-bit-aligned binary object
;; - a (bit-slice Binary Number Number), a bit-aligned sub-binary
;; - a (splice Number BitString BitString), a join of two bitstrings
;;
;; A BitString represents a string of bits, numbered ascending from
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

(module+ test
  (require typed/rackunit))

(provide bit-slice?
	 bit-slice-binary
	 bit-slice-low-bit
	 bit-slice-high-bit
	 splice?
	 splice-left
	 splice-right
	 BitString
	 bit-string?
	 bit-string-empty?
	 bit-string-length
	 bit-string-append
	 bit-string-split-at-or-false
	 bit-string-split-at
	 bit-string-take
	 bit-string-drop
	 bit-string-ref
	 sub-bit-string
	 bit-string-byte-count
	 copy-bits!
	 bit-string-pack!
	 bit-string-pack
	 bit-string->bytes
	 bit-string->bytes/align
	 bit-string->signed-integer
	 bit-string->unsigned-integer
	 bit-string->byte
	 bit-string->integer
	 integer->bit-string)

;; A section of a Bytes. Bits [low-bit,high-bit) from binary are
;; included in the slice.
(struct: bit-slice ([binary : Bytes] [low-bit : Natural] [high-bit : Natural])
	 #:transparent)

;; A join of two BitStrings. The length is maintained to improve the
;; time complexity of certain algorithms.
(struct: splice ([length : Natural] [left : BitString] [right : BitString])
	 #:transparent)

(define-type BitString (U Bytes bit-slice splice))
(define-predicate bit-string? BitString)

(: make-bit-slice : Bytes Natural Natural -> (U Bytes bit-slice))
(define (make-bit-slice binary low-bit high-bit)
  (if (= low-bit high-bit)
      #""
      (bit-slice binary low-bit high-bit)))

(module+ test
  (check-equal? (bit-string? "hello") #f)
  (check-equal? (bit-string? 123) #f)
  (check-equal? (bit-string? #"hello") #t)
  (check-equal? (bit-string? (make-bit-slice #"hello" 2 4)) #t)
  (check-equal? (bit-string? (splice 0 (bytes) (bytes))) #t))

(: bit-string-empty? : BitString -> Boolean)
;; True iff the given bitstring contains no bits.
(define (bit-string-empty? x)
  (zero? (bit-string-length x)))

(: bit-string-length : BitString -> Natural)
;; Returns the number of bits in the given bitstring.
(define (bit-string-length x)
  (cond
   ((bytes? x) (* 8 (bytes-length x)))
   ((bit-slice? x) (cast (- (bit-slice-high-bit x) (bit-slice-low-bit x)) Natural))
   ((splice? x) (splice-length x))))

(module+ test
  (check-equal? (bit-string-empty? (bytes)) #t)
  (check-equal? (bit-string-empty? (bytes 1)) #f)

  (check-equal? (bit-string-length (bytes)) 0)
  (check-equal? (bit-string-length (bytes 255)) 8)
  (check-equal? (bit-string-length (bytes 10)) 8)
  (check-equal? (bit-string-length (bit-slice (bytes) 0 0)) 0)
  (check-equal? (bit-string-length (make-bit-slice (bytes 255) 1 4)) 3)
  (check-equal? (bit-string-length (make-bit-slice (bytes 10) 1 4)) 3))

(: abutting? : BitString BitString -> Boolean)
;; True iff both arguments are slices of the same underlying Bytes,
;; and a's high-bit is the same as b's low-bit. Used to detect when an
;; underlying Bytes can safely be reused in an append operation.
(define (abutting? a b)
  (and (bit-slice? a)
       (bit-slice? b)
       (eq? (bit-slice-binary a)
	    (bit-slice-binary b))
       (= (bit-slice-high-bit a)
	  (bit-slice-low-bit b))))

(define abutting-test-binary (bytes 255))
(module+ test
  (check-equal? (abutting? abutting-test-binary abutting-test-binary) #f)
  (check-equal? (abutting? (bytes 255) (bytes 255)) #f)
  (check-equal? (abutting? (make-bit-slice abutting-test-binary 1 4)
                           (make-bit-slice (bytes 255) 4 6)) #f)
  (check-equal? (abutting? (make-bit-slice abutting-test-binary 1 4)
                           (make-bit-slice abutting-test-binary 4 6)) #t)
  (check-equal? (abutting? (make-bit-slice abutting-test-binary 1 4)
                           (make-bit-slice abutting-test-binary 5 6)) #f)
  (check-equal? (abutting? (make-bit-slice abutting-test-binary 1 3)
                           (make-bit-slice abutting-test-binary 4 6)) #f)
  (check-equal? (abutting? (make-bit-slice abutting-test-binary 4 6)
                           (make-bit-slice abutting-test-binary 1 4)) #f))

(: bit-string-append-2 : BitString BitString -> BitString)
;; 2ary BitString append operator.
(define (bit-string-append-2 a b)
  (if (and (bit-slice? a) ;; an inlined use of abutting?, because it
	   (bit-slice? b) ;; tries to assert the type of two values
	   (eq? (bit-slice-binary a)
		(bit-slice-binary b))
	   (= (bit-slice-high-bit a)
	      (bit-slice-low-bit b)))
      (if (= (bit-string-length (bit-slice-binary a))
	     (+ (bit-string-length a) (bit-string-length b)))
	  (bit-slice-binary a)
	  (make-bit-slice (bit-slice-binary a)
			  (bit-slice-low-bit a)
			  (bit-slice-high-bit b)))
      (let ((alen (bit-string-length a))
	    (blen (bit-string-length b)))
	(cond
	 [(zero? alen) b]
	 [(zero? blen) a]
	 [else (splice (+ alen blen) a b)]))))

(: bit-string-append : BitString * -> BitString)
;; Nary BitString append operator.
(define (bit-string-append . bss)
  (cond
   [(null? bss) #""]
   [(null? (cdr bss)) (car bss)]
   [(null? (cddr bss)) (bit-string-append-2 (car bss) (cadr bss))]
   [else (foldl (lambda: ([rhs : BitString] [lhs : BitString]) (bit-string-append-2 lhs rhs))
		(car bss)
		(cdr bss))]))

(module+ test
  (check-equal? (bit-string-append)
                #"")
  (check-equal? (bit-string-append abutting-test-binary)
                abutting-test-binary)
  (check-equal? (bit-string-append (make-bit-slice abutting-test-binary 0 4)
                                   (make-bit-slice abutting-test-binary 4 8))
                abutting-test-binary)
  (check-equal? (bit-string-append (make-bit-slice abutting-test-binary 0 4)
                                   (make-bit-slice abutting-test-binary 4 6)
                                   (make-bit-slice abutting-test-binary 6 8))
                abutting-test-binary)
  (check-equal? (bit-string-append (make-bit-slice abutting-test-binary 1 4)
                                   (make-bit-slice abutting-test-binary 4 6))
                (make-bit-slice abutting-test-binary 1 6))
  (check-equal? (bit-string-append (make-bit-slice (bytes 255) 1 4)
                                   (make-bit-slice (bytes 255) 4 6))
                (splice 5
                        (make-bit-slice (bytes 255) 1 4)
                        (make-bit-slice (bytes 255) 4 6)))

  (check-equal? (bit-string-length (bit-string-append (bytes 1) (bytes 2))) 16))

(: bit-string-split-at-or-false :
   BitString Integer -> (Values (Option BitString) (Option BitString)))
;; Returns the left- and right-hand portions of x split so that bit
;; number offset is excluded from the left hand side and included in
;; the right hand side. Returns (values #f #f) if offset is
;; out-of-range for x.
(define (bit-string-split-at-or-false x offset)
  (let ((len (bit-string-length x)))
    (if (or (negative? offset)
	    (> offset len))
	(values #f #f)
	(let: split : (Values BitString BitString)
	      ((x : BitString x)
	       (offset : Natural offset))
	  (cond
	   ((bytes? x)
	    (values (make-bit-slice x 0 offset)
		    (make-bit-slice x offset (* 8 (bytes-length x)))))
	   ((bit-slice? x)
	    (let ((bin (bit-slice-binary x))
		  (low (bit-slice-low-bit x))
		  (high (bit-slice-high-bit x)))
	      (let ((mid (+ low offset)))
		(values (make-bit-slice bin low mid)
			(make-bit-slice bin mid high)))))
	   ((splice? x)
	    (let* ((original-left (splice-left x))
		   (splice-midpoint (bit-string-length original-left)))
	      (cond
	       ((< offset splice-midpoint)
		(let-values (((left mid) (split original-left offset)))
		  (values left
			  (splice (cast (- (splice-length x) offset) Natural)
				  mid
				  (splice-right x)))))
	       ((= offset splice-midpoint)
		(values original-left (splice-right x)))
	       (else
		(let-values (((mid right) (split (splice-right x)
						 (cast (- offset splice-midpoint) Natural))))
		  (values (splice offset original-left mid)
			  right)))))))))))

(: bit-string-split-at : BitString Natural -> (Values BitString BitString))
;; As bit-string-split-at-or-false, but raises an exception if offset
;; is out-of-bounds instead of returning (values #f #f).
(define (bit-string-split-at x offset)
  (let-values (((lhs rhs) (bit-string-split-at-or-false x offset)))
    (if (or (not lhs) (not rhs))
	(error 'bit-string-split-at "Split point negative or beyond length of string: ~v" offset)
	(values lhs rhs))))

(: bit-string-take : BitString Natural -> BitString)
;; Retrieves the first offset bits of x. Raises an exception if offset is out-of-bounds.
(define (bit-string-take x offset)
  (define-values (left right) (bit-string-split-at x offset))
  left)

(: bit-string-drop : BitString Natural -> BitString)
;; Discards the first offset bits of x. Raises an exception if offset is out-of-bounds.
(define (bit-string-drop x offset)
  (define-values (left right) (bit-string-split-at x offset))
  right)

(: bit-string-ref : BitString Natural -> (U 0 1))
;; Retrieves a single bit at the given offset from the given bitstring.
;; Raises an exception if the offset is out-of-bounds.
(define (bit-string-ref x offset)
  (when (negative? offset)
    (error 'bit-string-ref "Offset must be non-negative: ~v" offset))
  (when (>= offset (bit-string-length x))
    (error 'bit-string-ref "Offset must be less than or equal to bit string length: ~v" offset))
  (let: search : (U 0 1)
	((x : BitString x)
	 (offset : Natural offset))
    (cond
     ((bytes? x)
      (let-values (((byte-offset bit-offset) (quotient/remainder offset 8)))
	(if (zero?
	     (bitwise-bit-field (bytes-ref x byte-offset) (- 7 bit-offset) (- 8 bit-offset)))
	    0 1)))
     ((bit-slice? x)
      (bit-string-ref (bit-slice-binary x) (+ (bit-slice-low-bit x) offset)))
     ((splice? x)
      (let* ((left (splice-left x))
	     (midpoint (bit-string-length left)))
	(if (< offset midpoint)
	    (search left offset)
	    (search (splice-right x) (cast (- offset midpoint) Natural))))))))

(module+ test
  (check-equal? (bit-string-ref (bytes #x80) 0) 1)
  (check-equal? (bit-string-ref (bytes #x80) 7) 0)
  (check-equal? (bit-string-ref (bytes #x20) 2) 1)
  (check-equal? (bit-string-ref (bytes #x01) 0) 0)
  (check-equal? (bit-string-ref (bytes #x01) 7) 1)
  (check-equal? (bit-string-ref (bytes #x00 #x80) 8) 1)
  (check-equal? (bit-string-ref (bytes #x00 #x01) 15) 1)
  (check-equal? (bit-string-ref (make-bit-slice (bytes #x20) 2 3) 0) 1)
  (check-equal? (bit-string-ref (make-bit-slice (bytes #x40) 2 3) 0) 0)

  ;; Caught by type (and contract):
  ;; (check-exn #rx"Offset must be non-negative"
  ;; 	   (lambda () (bit-string-ref (bytes #xff) -1)))
  (check-exn #rx"Offset must be less than or equal to bit string length"
             (lambda () (bit-string-ref (bytes #xff) 100))))


(: sub-bit-string : BitString Natural Natural -> BitString)
;; Retrieves a section of the given bitstring, starting and ending at
;; the given offsets. low-bit is inclusive, high-bit exclusive.
;; Raises an exception if low-bit or high-bit are out of bounds.
(define (sub-bit-string x low-bit high-bit)
  (when (negative? low-bit)
    (error 'sub-bit-string "Low bit must be non-negative: ~v" low-bit))
  (when (> high-bit (bit-string-length x))
    (error 'sub-bit-string
	   "High bit must be less than or equal to bit string length: ~v" high-bit))
  (define delta (- high-bit low-bit))
  (cond
   ((negative? delta)
    (error 'sub-bit-string "High bit ~v must be greater than or equal to low bit ~v"
	   high-bit low-bit))
   ((bytes? x)
    (if (and (zero? low-bit)
	     (= high-bit (* 8 (bytes-length x))))
	x
	(make-bit-slice x low-bit high-bit)))
   ((bit-slice? x)
    (let ((old-low (bit-slice-low-bit x)))
      (make-bit-slice (bit-slice-binary x)
		      (+ old-low low-bit)
		      (+ old-low high-bit))))
   ((splice? x)
    (let-values (((left mid) (bit-string-split-at x low-bit)))
      (let-values (((mid right) (bit-string-split-at mid delta)))
	mid)))))

(: bits->bytes : Natural -> Natural)
;; Returns the smallest number of whole bytes having together no fewer
;; than bit-count bits.
(define (bits->bytes bit-count)
  (quotient (+ 7 bit-count) 8))

(: bit-string-byte-count : BitString -> Natural)
;; Returns the smallest number of whole bytes long enough to contain
;; all the bits in the argument.
(define (bit-string-byte-count x)
  (bits->bytes (bit-string-length x)))

(module+ test
  (check-equal? (bit-string-byte-count (bytes #xff)) 1)
  (check-equal? (bit-string-byte-count (bytes #xff #x00)) 2)
  (check-equal? (bit-string-byte-count (make-bit-slice (bytes #xff #x00) 6 16)) 2)
  (check-equal? (bit-string-byte-count (make-bit-slice (bytes #xff #x00) 6 14)) 1))

(: bits->bytes+slop : Natural -> (Values Natural Natural))
;; As for bits->bytes, but also returns, as the second value, the
;; number of bits "left over" in the resulting number of bytes.
(define (bits->bytes+slop bit-count)
  (let* ((byte-count (quotient (+ 7 bit-count) 8))
	 (slop (- (* 8 byte-count) bit-count)))
    (values byte-count (cast slop Natural))))

(: bit-string-byte-count+slop : BitString -> (Values Natural Natural))
;; Is to bit-string-byte-count as bits->bytes+slop is to bits->bytes.
(define (bit-string-byte-count+slop x)
  (bits->bytes+slop (bit-string-length x)))

(module+ test
  (check-equal? (let-values (((b s) (bit-string-byte-count+slop
                                     (make-bit-slice (bytes #xff #x00) 6 16))))
                  (list b s))
                (list 2 6)))

(: bit-mask : Natural -> Natural)
;; Returns the sum of 2^0 ... 2^(width-1).
(define (bit-mask width)
  (cast (sub1 (arithmetic-shift 1 width)) Natural))

(: copy-bits! : Bytes Natural Bytes Natural Natural -> Void)
;; Destructively updates its first argument, replacing bits numbered
;; [target-offset, target-offset + remaining-count) with bits numbered
;; [source-offset, source-offset + remaining-count) taken from source.
(define (copy-bits! target target-offset source source-offset remaining-count)
  (let-values (((target-byte target-shift) (quotient/remainder target-offset 8))
	       ((source-byte source-shift) (quotient/remainder source-offset 8)))
    (: bump-target! : Natural -> Void)
    (define (bump-target! bits)
      (let-values (((whole-bytes remaining-bits) (quotient/remainder (+ target-shift bits) 8)))
	(set! target-byte (+ target-byte whole-bytes))
	(set! target-shift remaining-bits)))
    (: bump-source! : Natural -> Void)
    (define (bump-source! bits)
      (let-values (((whole-bytes remaining-bits) (quotient/remainder (+ source-shift bits) 8)))
	(set! source-byte (+ source-byte whole-bytes))
	(set! source-shift remaining-bits)))
    (: shuffle! : Natural -> Void)
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
	(set! remaining-count (cast (- remaining-count bit-count) Natural))
	(bump-target! bit-count)
	(bump-source! bit-count)))
    ;; First, align the source to a byte boundary.
    (when (positive? source-shift)
      (let* ((source-left-overlap (min remaining-count (cast (- 8 source-shift) Natural)))
	     (remaining-in-first-target-byte (cast (- 8 target-shift) Natural))
	     (delta (- source-left-overlap remaining-in-first-target-byte)))
	(if (positive? delta)
	    (begin (shuffle! remaining-in-first-target-byte)
		   (shuffle! delta))
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
	    (shuffle! (cast (- 8 target-shift) Natural))
	    (shuffle! (cast (- 8 source-shift) Natural)))))
    ;; Now we have fewer than eight bits left to transfer.
    (when (positive? remaining-count)
      (let ((remaining-in-first-target-byte (cast (- 8 target-shift) Natural)))
	(if (> remaining-count remaining-in-first-target-byte)
	    (begin (shuffle! remaining-in-first-target-byte)
		   (shuffle! remaining-count))
	    (shuffle! remaining-count))))
    ;; We're finally done.
    ))

(module+ test
  (check-equal? (let ((buf (bytes 0))) (copy-bits! buf 4 (bytes 255 255 255) 17 4) buf)
                (bytes 15))
  (check-equal? (let ((buf (bytes 0 0 0 0)))
                  (copy-bits! buf 6 (bytes 255 255 255 255) 2 12)
                  buf)
                (bytes #b00000011 #b11111111 #b11000000 #b00000000)))

(: bit-string-pack! : BitString Bytes Natural -> Void)
;; Copies the whole of x into buf, so that when it returns,
;; (sub-bit-string buf offset (+ offset (bit-string-length x)))
;; is equal to x.
(define (bit-string-pack! x buf offset)
  (cond
   ((bytes? x)
    (copy-bits! buf offset x 0 (* 8 (bytes-length x))))
   ((bit-slice? x)
    (copy-bits! buf offset
		(bit-slice-binary x) (bit-slice-low-bit x)
		(cast (- (bit-slice-high-bit x) (bit-slice-low-bit x)) Natural)))
   ((splice? x)
    (let* ((left (splice-left x))
	   (left-length (bit-string-length left)))
      (bit-string-pack! left buf offset)
      (bit-string-pack! (splice-right x) buf (+ offset left-length))))))

(module+ test
  (check-equal? (let ((buf (bytes 0 0 0 0)))
                  (bit-string-pack! (bytes 255) buf 4)
                  buf)
                (bytes 15 240 0 0)))

(: flatten-to-bytes : BitString Boolean -> (U Bytes bit-slice))
;; Returns a BitString logically identical to its argument, but
;; physically stored in a contiguous underlying Bytes. If align-right?
;; is false, padding zeros (if any) will appear at the highest bits in
;; the resulting byte array; if align-right? is true, padding zeros
;; will appear at the lowest bits in the resulting byte array.
(define (flatten-to-bytes x align-right?)
  (let-values (((byte-count bits-remaining)
		(bit-string-byte-count+slop x)))
    (let ((buf (make-bytes byte-count 0)))
      (bit-string-pack! x buf (if align-right? bits-remaining 0))
      (if (zero? bits-remaining)
	  buf
	  (make-bit-slice buf 0 (bit-string-length x))))))

(: bit-string-pack : BitString -> BitString)
;; Returns a BitString logically identical to its argument, but
;; physically stored in a contiguous underlying Bytes. Padding zeros
;; (if any) will appear at the highest bits in the resulting byte
;; array.
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

(module+ test
  (check-equal? (bit-string-pack (bytes 1)) (bytes 1))
  (check-equal? (bit-string-pack (make-bit-slice (bytes 255 255) 2 14))
                (make-bit-slice (bytes 255 255) 2 14))
  (check-equal? (bit-string-pack (make-bit-slice (bytes 255 255) 2 4))
                (make-bit-slice (bytes 192) 0 2)))

(: bit-string->bytes : BitString -> Bytes)
;; Equivalent to (bit-string->bytes/align x #f). (See below.)
(define (bit-string->bytes x)
  (bit-string->bytes/align x #f))

(: bit-string->bytes/align : BitString Boolean -> Bytes)
;; Returns a Bytes equivalent to x, padded if necessary with zero bits
;; to the left if align-right? is true, or to the right if
;; align-right? is false.
(define (bit-string->bytes/align x align-right?)
  (if (bytes? x)
      x
      (let ((v (flatten-to-bytes x align-right?)))
	(if (bit-slice? v)
	    (bit-slice-binary v)
	    v))))

(module+ test
  ;; 1111 1111 1111 0000 0000 0000
  ;;        -- ---- ----
  ;;
  ;; packed into bytes, yielding two bytes worth, and per the rules
  ;; described above, the bits are copied left to right, so
  ;;
  ;; 1111 1100 0000 0000
  ;; ---- ---- --
  (check-equal? (bit-string-pack (make-bit-slice (bytes 255 240 0) 6 16))
                (make-bit-slice (bytes 252 0) 0 10))
  (check-equal? (bit-string->bytes (make-bit-slice (bytes 255 240 0) 6 16))
                (bytes 252 0))

  ;; Aligned right, that'll be
  ;; 0000 0011 1111 0000
  ;;        -- ---- ----
  (check-equal? (bit-string->bytes/align (make-bit-slice (bytes 255 240 0) 6 16) #t)
                (bytes 3 240)))

(: bit-string->signed-integer : BitString Boolean -> Integer)
;; Extract an arbitrary-width two's-complement integer from a
;; bitstring. Big-endian byte ordering is used iff big-endian? is
;; true.
(define (bit-string->signed-integer x big-endian?)
  (let ((width (bit-string-length x))
	(value (bit-string->unsigned-integer x big-endian?)))
    (if (< value (arithmetic-shift 1 (sub1 width)))
	value
	(- value (arithmetic-shift 1 width)))))

(: bit-string->unsigned-integer : BitString Boolean -> Nonnegative-Integer)
;; Extract an arbitrary-width unsigned integer from a bitstring.
;; Big-endian byte ordering is used iff big-endian? is true.
(define (bit-string->unsigned-integer x big-endian?)
  (let ((width (bit-string-length x)))
    (if big-endian?
	(let* ((bs (bit-string->bytes/align x #t))
	       (count (bytes-length bs)))
	  (do ((i 0 (+ i 1))
	       (#{acc : Nonnegative-Integer} 0 (bitwise-ior (arithmetic-shift acc 8)
							    (bytes-ref bs i))))
	      ((= i count) acc)))
	(let* ((bs (bit-string->bytes/align x #f))
	       (count (bytes-length bs)))
	  (do ((i (- count 1) (- i 1))
	       (#{acc : Nonnegative-Integer} 0 (bitwise-ior (arithmetic-shift acc 8)
							    (bytes-ref bs i))))
	      ((< i 0) acc))))))

(: bit-string->byte : BitString -> Byte)
;; Converts an eight-bit-wide bit string into a byte.
(define (bit-string->byte x)
  (when (not (= (bit-string-length x) 8))
    (error 'bit-string->byte "Expects a bit string of length 8 bits: ~v" x))
  (bytes-ref (bit-string->bytes x) 0))

(module+ test
  (check-equal? (bit-string->byte (bytes 1)) 1)
  (check-equal? (bit-string->byte (bytes #xff)) #xff)
  (check-equal? (bit-string->byte (make-bit-slice (bytes 255 240 0) 6 14)) 252)

  (check-exn #rx"Expects a bit string of length 8 bits"
             (lambda () (bit-string->byte (make-bit-slice (bytes 255 240 0) 6 16)))))

(: bit-string->integer : BitString Boolean Boolean -> Integer)
;; Generic version of the above.
(define (bit-string->integer x big-endian? signed?)
  (if signed?
      (bit-string->signed-integer x big-endian?)
      (bit-string->unsigned-integer x big-endian?)))

(module+ test
  (check-equal? (bit-string->integer (bytes 1 2 3 4) #t #f) #x01020304)
  (check-equal? (bit-string->integer (bytes 129 2 3 4) #t #f) #x81020304)
  (check-equal? (bit-string->integer (bytes 129 2 3 4) #t #t) (- #x81020304 #x100000000))
  (check-equal? (bit-string->integer (bytes 1 2 3 4) #f #f) #x04030201)
  (check-equal? (bit-string->integer (bytes 1 2 3 132) #f #f) #x84030201)
  (check-equal? (bit-string->integer (bytes 1 2 3 132) #f #t) (- #x84030201 #x100000000))

  (check-equal? (bit-string->integer (make-bit-slice (bytes 255 240 0) 6 16) #f #f) 252)
  (check-equal? (bit-string->integer (make-bit-slice (bytes 255 240 0) 6 16) #f #t) 252)
  (check-equal? (bit-string->integer (make-bit-slice (bytes 255 240 0) 6 16) #t #f) 1008)
  (check-equal? (bit-string->integer (make-bit-slice (bytes 255 240 0) 6 16) #t #t) -16))

(: integer->bit-string : Integer Natural Boolean -> BitString)
;; Encodes an integer as a BitString of a given width using the given
;; byte ordering. Signedness doesn't matter here - it only matters
;; for decoding ints from bitstrings.
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

(module+ test
  (check-equal? (integer->bit-string #x01020304 32 #t) (bytes 1 2 3 4))
  (check-equal? (integer->bit-string #x81020304 32 #t) (bytes 129 2 3 4))
  (check-equal? (integer->bit-string (- #x81020304 #x100000000) 32 #t) (bytes 129 2 3 4))
  (check-equal? (integer->bit-string #x04030201 32 #f) (bytes 1 2 3 4))
  (check-equal? (integer->bit-string #x84030201 32 #f) (bytes 1 2 3 132))
  (check-equal? (integer->bit-string (- #x84030201 #x100000000) 32 #f) (bytes 1 2 3 132))

  (check-equal? (integer->bit-string 252 10 #f)  (make-bit-slice (bytes 252 0) 0 10))
  (check-equal? (integer->bit-string 1008 10 #t) (make-bit-slice (bytes 3 240) 6 16))
  (check-equal? (integer->bit-string -16 10 #t)  (make-bit-slice (bytes 255 240) 6 16))
  ;;                                                                    ^^^
  ;; That this is not 3 is insignificant. The bit-slice says that bits number 0-5 are
  ;; not part of the answer.

  (check-exn #rx"Split point negative or beyond length of string"
             (lambda ()
               (bit-string-split-at (bytes #xff) 100)
               (void)))

  (check-equal? (let-values (((a b) (bit-string-split-at-or-false (bytes 1) -2)))
                  (or a b))
                #f)
  (check-equal? (let-values (((a b) (bit-string-split-at-or-false (bytes 1) 2)))
                  (and a b
                       (equal? (bit-string->bytes/align a #t) (bytes 0))
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

  ;; Caught by type (and contract):
  ;; (check-exn #rx"Low bit must be non-negative"
  ;; 	   (lambda () (sub-bit-string (bytes 255) -1 6)))

  (check-exn #rx"High bit must be less than or equal to bit string length"
             (lambda () (sub-bit-string (bytes 255) 1 60)))

  (check-exn #rx"High bit 1 must be greater than or equal to low bit 6"
             (lambda () (sub-bit-string (bytes 255) 6 1)))

  (check-equal? (bit-string->bytes
                 (sub-bit-string (bit-string-append (bytes 1) (bytes 2)) 0 8))
                (bytes 1))
  (check-equal? (bit-string-pack (sub-bit-string
                                  (bit-string-append (bytes 1)
                                                     (make-bit-slice (bytes 255) 1 6))
                                  4 12))
                (bytes 31))
  (check-equal? (bit-string-append #"" #"") #"")
  (check-equal? (bit-string-append #"a" #"") #"a")
  (check-equal? (bit-string-append #"" #"a") #"a"))
