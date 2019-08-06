#lang racket

;; Example from the Erlang Programming Examples section of the Erlang User's Guide:
;;
;; -define(IP_VERSION, 4).
;; -define(IP_MIN_HDR_LEN, 5).
;; DgramSize = byte_size(Dgram),
;; case Dgram of
;;     <<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16,
;;       ID:16, Flgs:3, FragOff:13,
;;       TTL:8, Proto:8, HdrChkSum:16,
;;       SrcIP:32,
;;       DestIP:32, RestDgram/binary>> when HLen>=5, 4*HLen=<DgramSize ->
;;         OptsLen = 4*(HLen - ?IP_MIN_HDR_LEN),
;;         <<Opts:OptsLen/binary,Data/binary>> = RestDgram,
;;     ...
;; end.
;;
;; Translated into the syntax defined in this file:
;;
;; (define IP-VERSION 4)
;; (define IP-MINIMUM-HEADER-LENGTH 5)
;; (bit-string-case datagram
;;   ([ (= IP-VERSION :: bits 4)
;;      (header-length :: bits 4)
;;      service-type
;;      (total-length :: bits 16)
;;      (id :: bits 16)
;;      (flags :: bits 3)
;;      (fragment-offset :: bits 13)
;;      ttl
;;      protocol
;;      (header-checksum :: bits 16)
;;      (source-ip :: bits 32)
;;      (destination-ip :: bits 32)
;;      (rest :: binary) ]
;;    (when (and (>= header-length 5)
;;               (>= (bit-string-length datagram) (* header-length 4))))
;;    (let ((options-length (* 4 (- header-length IP-MINIMUM-HEADER-LENGTH))))
;;      (bit-string-case rest
;;        ([ (opts :: binary bytes options-length)
;;           (data :: binary) ]
;;         ...))))
;;   ...)

;; TODO: this should probably be a match extension

(require "bitstx.rkt")
(require "bitstring.rkt")
(require (for-syntax (only-in typed/untyped-utils syntax-local-typed-context?)))
(require (prefix-in typed: typed/racket/base))

(provide :: bit-string-case)

(define-syntax (default-on-short stx)
  (syntax-case stx ()
    [(_)
     (if (syntax-local-typed-context?)
         #'(let ()
             (typed:: h (typed:All (a) (typed:-> (typed:-> a) a)))
             (define (h fail) (fail))
             h)
         #'(lambda (fail) (fail)))]))

(define-syntax bit-string-case
  (syntax-rules ()
    ((_ value #:on-short kshort-exp clause ...)
     (let ((temp value)
           (kshort kshort-exp))
       (when (not (bit-string? temp))
	 (error 'bit-string-case "Not a bit string: ~v" temp))
       (bit-string-case-helper temp kshort clause ...)))
    ((_ value clause ...)
     (bit-string-case value #:on-short (default-on-short) clause ...))))

(define-for-syntax (parse-options action options)
  (let loop ((options options)
	     (type (syntax integer))
	     (signedness (syntax unsigned))
	     (endianness (syntax big-endian))
	     (width (syntax default)))
    (syntax-case options (binary integer float
			  signed unsigned
			  little-endian big-endian native-endian
			  bytes bits default)
      (()
       #`(#,action #,type #,signedness #,endianness #,width))
      ((binary rest ...)
       (loop (syntax (rest ...)) (syntax binary) signedness endianness width))
      ((integer rest ...)
       (loop (syntax (rest ...)) (syntax integer) signedness endianness width))
      ((float rest ...)
       (loop (syntax (rest ...)) (syntax float) signedness endianness width))
      ((signed rest ...)
       (loop (syntax (rest ...)) type (syntax signed) endianness width))
      ((unsigned rest ...)
       (loop (syntax (rest ...)) type (syntax unsigned) endianness width))
      ((big-endian rest ...)
       (loop (syntax (rest ...)) type signedness (syntax big-endian) width))
      ((little-endian rest ...)
       (loop (syntax (rest ...)) type signedness (syntax little-endian) width))
      ((native-endian rest ...)
       (loop (syntax (rest ...)) type signedness (syntax native-endian) width))
      ((bytes n rest ...)
       (loop (syntax (rest ...)) type signedness endianness (syntax (* 8 n))))
      ((bits n rest ...)
       (loop (syntax (rest ...)) type signedness endianness (syntax n)))
      ((default rest ...)
       (loop (syntax (rest ...)) type signedness endianness (syntax default))))))

(define-for-syntax (parse-trailer action trailer)
  (syntax-case trailer (::)
    ((:: (parser-fn-or-macro arg ...))
     #`(#,action (parser-fn-or-macro arg ...) invalid invalid invalid))
    ((:: option ...)
     (parse-options action (syntax (option ...))))
    (()
     (parse-options action (syntax ())))))

(define-for-syntax (parse-bit-string-pattern pattern-clause)
  (syntax-case pattern-clause (= ::)
    ((= expr trailer ...)
     (parse-trailer (syntax (check expr)) (syntax (trailer ...))))
    ((:: trailer ...)
     (parse-trailer (syntax discard) (syntax (:: trailer ...))))
    ((id trailer ...)
     (parse-trailer (syntax (bind id)) (syntax (trailer ...))))
    (::
     (raise-syntax-error 'bit-string-case
                         "\"::\" cannot be used as binding-pattern id"))
    (=
     (raise-syntax-error 'bit-string-case
                         "\"=\" used outside of comparison-pattern"))
    (id
     (parse-trailer (syntax (bind id)) (syntax ())))))

(define-syntax bit-string-case-helper
  (lambda (stx)
    (syntax-case stx (when else)
      ((_ value kshort (else body ...))
       (syntax (begin body ...)))
      ((_ value kshort)
       (syntax (error 'bit-string-case "No matching clauses for ~v"
		      (bit-string-pack value))))
      ((_ value kshort ((pattern-clause ...) body-and-guard ...) clause ...)
       (with-syntax ([tval (syntax-case (syntax (body-and-guard ...)) (when else)
			     (((when guard-exp) body ...)
			      (syntax (if guard-exp (begin body ...) (kf))))
			     ((body ...)
			      (syntax (begin body ...))))]
		     [canonical-pattern (map parse-bit-string-pattern
					     (syntax->list (syntax (pattern-clause ...))))])
	 (syntax
	  (let ((kf (lambda ()
		      (bit-string-case-helper value kshort clause ...))))
	    (bit-string-case-arm value
				 tval
				 kf
                                 kshort
				 canonical-pattern))))))))

(define-syntax bit-string-case-arm
  (lambda (stx)
    (syntax-case stx (binary integer float default)
      ((_ value tval fthunk kshort ())
       #'(if (zero? (bit-string-length value))
             tval
             (fthunk)))
      ((_ value tval fthunk kshort (( action (parser arg ...) dontcare1 dontcare2 dontcare3 )
                                    remaining-clauses ...))
       #'(parser #t
                 value
                 (lambda (result remaining-input)
                   ;; TODO: support separation of transformation
                   ;; from parsing so expensive transforms can
                   ;; all be done together at the end.
                   (bit-string-perform-action action result fthunk
                                              (bit-string-case-arm remaining-input
                                                                   tval fthunk kshort
                                                                   (remaining-clauses ...))))
                 (lambda ([short? #f]) (if short? (kshort fthunk) (fthunk)))
                 arg ...))
      ((_ value tval fthunk kshort (( action binary dontcare1 dontcare2 default ) ))
       #'(bit-string-perform-action action value fthunk tval))
      ((_ value tval fthunk kshort (( action binary dontcare1 dontcare2 default )
                                    remaining-clause remaining-clauses ...))
       (raise-syntax-error 'bit-string-case
                           "Clauses supplied after variable-width binary clause"))
      ((_ value tval fthunk kshort (( action integer dontcare1 dontcare2 default )
                                    remaining-clauses ...))
       #'(bit-string-case-arm value tval fthunk kshort (( action integer dontcare1 dontcare2 8 )
                                                        remaining-clauses ...)))
      ((_ value tval fthunk kshort (( action float dontcare1 dontcare2 default )
                                    remaining-clauses ...))
       #'(bit-string-case-arm value tval fthunk kshort (( action float dontcare1 dontcare2 64 )
                                                        remaining-clauses ...)))
      ((_ value tval fthunk kshort (( action type signedness endianness width )
                                    remaining-clauses ...))
       #'(let-values (((lhs rhs) (bit-string-split-at-or-false value width)))
           (if (or (not lhs) (not rhs))
               (kshort fthunk)
               (let ((this-value (bit-string-case-extract-value
                                  lhs type signedness endianness width)))
                 (bit-string-perform-action action this-value fthunk
                                            (bit-string-case-arm rhs
                                                                 tval fthunk kshort
                                                                 (remaining-clauses ...))))))))))

(define-syntax bit-string-perform-action
  (syntax-rules (check bind discard)
    ((_ (bind identifier) this-value fthunk tval)
     (let ((identifier this-value))
       tval))
    ((_ (check expr) this-value fthunk tval)
     (if (bit-string-case-equal? this-value expr)
	 tval
	 (fthunk)))
    ((_ discard this-value fthunk tval)
     tval)))

(define-syntax bit-string-case-extract-value
  (syntax-rules (binary integer float signed unsigned)
    ((_ bin binary dontcare1 dontcare2 width-in-bits)
     ;; The width is already correct from the action of bit-string-split-at-or-false.
     bin)
    ((_ bin float dontcare1 endianness 32)
     (floating-point-bytes->real (bit-string->bytes bin)
				 (bit-string-case-endianness endianness)
				 0 4))
    ((_ bin float dontcare1 endianness 64)
     (floating-point-bytes->real (bit-string->bytes bin)
				 (bit-string-case-endianness endianness)
				 0 8))
    ((_ bin integer unsigned endianness 8)
     ;; The width is already correct from the action of bit-string-split-at-or-false.
     (bit-string->byte bin))
    ((_ bin integer unsigned endianness width-in-bits)
     ;; The width is already correct from the action of bit-string-split-at-or-false.
     (bit-string->unsigned-integer bin (bit-string-case-endianness endianness)))
    ((_ bin integer signedness endianness width-in-bits)
     ;; The width is already correct from the action of bit-string-split-at-or-false.
     (bit-string->signed-integer bin (bit-string-case-endianness endianness)))))

(define-syntax bit-string-case-endianness
  (syntax-rules (little-endian big-endian native-endian)
    ((_ little-endian)
     #f)
    ((_ big-endian)
     #t)
    ((_ native-endian)
     (system-big-endian?))))
