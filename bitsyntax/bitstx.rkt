#lang racket

(provide ::)

(define-syntax ::
  (lambda (stx)
    (raise-syntax-error #f "Illegal use of :: outside bit-string-case or bit-string" stx)))
