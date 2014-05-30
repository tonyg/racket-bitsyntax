#lang racket/base

;; (bit-string (IP-VERSION :: bits 4)
;;             (header-length :: bits 4)
;;             service-type
;;             (total-length :: bits 16)
;;             (id :: bits 16)
;;             (flags :: bits 3)
;;             (fragment-offset :: bits 13)
;;             ttl
;;             protocol
;;             (header-checksum :: bits 16)
;;             (source-ip :: bits 32)
;;             (destination-ip :: bits 32)
;;             (rest :: binary))

(require "bitstx.rkt")
(require "bitstring.rkt")

(provide :: bit-string)

(define-syntax bit-string
  (syntax-rules ()
    ((_)
     (bytes))
    ((_ spec)
     (parse-spec spec))
    ((_ spec0 specs ...)
     (bit-string-append (parse-spec spec0) (bit-string specs ...)))))

(define-syntax parse-spec
  (syntax-rules (::)
    ((_ (expr :: (parser-fn-or-macro arg ...)))
     (let ((temp expr))
       (parser-fn-or-macro #f temp arg ...)))
    ((_ (expr :: option ...))
     (parse-options (option ...) expr))
    ((_ expr)
     (parse-options () expr))))

(define-syntax parse-options
  (syntax-rules (binary integer float
		 little-endian big-endian native-endian
		 bytes bits default)
    ((_ options value)
     (parse-options options
		    value
		    integer
		    big-endian
		    default))
    ((_ () value type endianness width-in-bits)
     (build-bit-string-segment value type endianness width-in-bits))

    ((_      (binary rest ...) value type    endianness width-in-bits)
     (parse-options (rest ...) value binary  endianness width-in-bits))
    ((_     (integer rest ...) value type    endianness width-in-bits)
     (parse-options (rest ...) value integer endianness width-in-bits))
    ((_       (float rest ...) value type    endianness width-in-bits)
     (parse-options (rest ...) value float   endianness width-in-bits))

    ((_ (little-endian rest ...) value type endianness    width-in-bits)
     (parse-options   (rest ...) value type little-endian width-in-bits))
    ((_    (big-endian rest ...) value type endianness    width-in-bits)
     (parse-options   (rest ...) value type big-endian    width-in-bits))
    ((_ (native-endian rest ...) value type endianness    width-in-bits)
     (parse-options   (rest ...) value type native-endian width-in-bits))

    ((_     (bytes n rest ...) value type endianness width-in-bits)
     (parse-options (rest ...) value type endianness (* 8 n)))
    ((_      (bits n rest ...) value type endianness width-in-bits)
     (parse-options (rest ...) value type endianness n))
    ((_     (default rest ...) value type endianness width-in-bits)
     (parse-options (rest ...) value type endianness default))))

(define-syntax build-bit-string-segment
  (syntax-rules (binary integer float default)
    ((_ value binary dontcare1 default)
     value)
    ((_ value binary dontcare1 width-in-bits)
     (let-values (((lhs rhs) (bit-string-split-at value width-in-bits)))
       lhs))
    ((_ value float endianness default)
     (build-bit-string-segment value float endianness 64))
    ((_ value float endianness 32)
     (real->floating-point-bytes value 4 (build-bit-string-endianness endianness)))
    ((_ value float endianness 64)
     (real->floating-point-bytes value 8 (build-bit-string-endianness endianness)))
    ((_ value integer endianness default)
     (build-bit-string-segment value integer endianness 8))
    ((_ value integer endianness width-in-bits)
     (integer->bit-string value width-in-bits (build-bit-string-endianness endianness)))))

(define-syntax build-bit-string-endianness
  (syntax-rules (little-endian big-endian native-endian)
    ((_ little-endian)
     #f)
    ((_ big-endian)
     #t)
    ((_ native-endian)
     (system-big-endian?))))
