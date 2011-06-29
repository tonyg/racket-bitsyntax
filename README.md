# racket-bitsyntax

Adds library support for slices and splices of byte-vectors to Racket,
and adds syntax for pattern-matching such bit-strings, inspired by
Erlang's binary pattern-matching:

 - <http://www.erlang.org/doc/reference_manual/expressions.html#id77409>
 - <http://www.erlang.org/doc/programming_examples/bit_syntax.html>

Here's a Racket equivalent of the example given in the Erlang documentation:

    (define IP-VERSION 4)
    (define IP-MINIMUM-HEADER-LENGTH 5)
    (bit-string-case datagram
      ([ (= IP-VERSION : bits 4)
         (header-length : bits 4)
         service-type
         (total-length : bits 16)
         (id : bits 16)
         (flags : bits 3)
         (fragment-offset : bits 13)
         ttl
         protocol
         (header-checksum : bits 16)
         (source-ip : bits 32)
         (destination-ip : bits 32)
         (rest : binary) ]
       (when (and (>= header-length 5)
                  (>= (bit-string-length datagram) (* header-length 4))))
       (let ((options-length (* 4 (- header-length IP-MINIMUM-HEADER-LENGTH))))
         (bit-string-case rest
           ([ (opts : binary bytes options-length)
              (data : binary) ]
            'datagram-valid))))
      [else
       'datagram-not-valid])
