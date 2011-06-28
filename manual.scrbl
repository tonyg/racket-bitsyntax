#lang scribble/manual

@(require planet/scribble
	  scribble/racket
	  (for-label racket
		     "main.rkt"))

@title{racket-bitsyntax}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@local-table-of-contents[]

If you find that this library lacks some feature you need, or you have
a suggestion for improving it, please don't hesitate to
@link["mailto:tonygarnockjones@gmail.com"]{get in touch with me}!

@section{Introduction}

This library adds three features to Racket:

@itemize[
  @item{library support for @italic{bit strings}, a generalization of
  byte vectors;
  }

  @item{syntactic support for extracting integers, floats and
  sub-bit-strings from bit strings;
  and}

  @item{syntactic support for constructing bit strings from integers,
  floats and other bit strings.}
]

It is heavily inspired by Erlang's binaries, bitstrings, and binary
pattern-matching. The Erlang documentation provides a good
introduction to these features:

@itemize[
  @item{@link["http://www.erlang.org/doc/reference_manual/expressions.html#id77409"]{Bit
  syntax expressions} in the Erlang Reference Manual}

  @item{@link["http://www.erlang.org/doc/programming_examples/bit_syntax.html"]{Bit
  syntax} in the Programming Examples Manual}
]

@section{What is a bit string?}

A bit string is either

@itemize[
  @item{a byte vector, as returned by @racket[bytes] and friends;
  }

  @item{a bit-resolution slice of a byte vector, as returned by
  @racket[sub-bit-string];
  or}

  @item{a splicing-together of two bit strings, as returned by
  @racket[bit-string-append].}
]

The routines in this library are written, except where specified, to
handle any of these three representations for bit strings.

If you need to flatten a bit string into a contiguous sequence of
whole bytes, use @racket[bit-string->bytes] or
@racket[bit-string->bytes/align].

@section{API}

All the functionality below can be accessed with a single
@racket[require]:

@(defmodule/this-package main)

@subsection{Pattern-matching bit strings}

@defform/subs[#:literals (when else
			  = ? :discard
			  :binary :integer :float
			  :signed :unsigned
			  :little-endian :big-endian :native-endian
			  :bytes :bits :default)
	      (bit-string-case value-expr clause ...)
	      ((clause ([bit-string-segment-pattern ...]
			(when guard-expr)
			body-expr ...)
		       ([bit-string-segment-pattern ...]
			body-expr ...)
		       (else
			body-expr ...))
	       (bit-string-segment-pattern
		(:discard maybe-type maybe-signedness maybe-endianness maybe-width)
		(= expr maybe-type maybe-signedness maybe-endianness maybe-width)
		(? id maybe-type maybe-signedness maybe-endianness maybe-width))
	       (maybe-type code:blank
			   (code:line :integer)
			   (code:line :float)
			   (code:line :binary))
	       (maybe-signedness code:blank
				 (code:line :signed)
				 (code:line :unsigned))
	       (maybe-endianness code:blank
				 (code:line :big-endian)
				 (code:line :little-endian)
				 (code:line :native-endian))
	       (maybe-width code:blank
			    (code:line :bytes integer)
			    (code:line :bits integer)
			    (code:line :default)))]{

The @racket[value-expr] is evaluated first. It must evaluate to a bit
string---any object for which @racket[bit-string?] would return
@racket[#t].

Each @racket[clause] is then tried in turn. The first succeeding
clause determines the result of the whole expression. A clause matches
successfully if all its @racket[bit-string-segment-pattern]s match
some portion of the input, there is no unused input left over at the
end, and the @racket[guard-expr] (if there is one) evaluates to a true
value. If a @racket[clause] succeeds, then @racket[(begin body-expr
...)] is evaluated, and its result becaomes the result of the whole
expression.

If none of the @racket[clause]s succeed, and there is an @racket[else]
clause, its @racket[body-expr]s are evaluated and returned. If there's
no @racket[else] clause and none of the others succeed, an error is
signalled.

Each @racket[bit-string-segment-pattern] matches zero or more
@italic{bits} of the input bit string. The given type, signedness,
endianness and width are used to extract a value from the bit string,
at which point it is either compared to some other value (if
@racket[(= expr)] was used in the segment-pattern), bound to a pattern
variable (if @racket[(? id)] was used), or discarded (if
@racket[:discard] was used) before matching continues with the next
@racket[bit-string-segment-pattern].

The supported segment types are

@itemize[

  @item{@racket[:integer] -- this is the default. A signed or
  unsigned, big- or little-endian integer of the given width in bits is
  read out of the bit string. Unless otherwise specified, integers
  default to big-endian, unsigned, and eight bits wide. Any width, not
  just multiples of eight, is supported.}

  @item{@racket[:float] -- A 32- or 64-bit float in either big- or
  little-endian byte order is read out of the bit string using
  @racket[floating-point-bytes->real]. Unless otherwise specified,
  floats default to big-endian and 64 bits wide. Widths other than 32 or
  64 bits are unsupported.}

  @item{@racket[:binary] -- A sub-bit-string is read out of the bit
  string. The bit string can be an arbitrary number of bits long, not
  just a multiple of eight. Unless otherwise specified, the entire rest
  of the input will be consumed and returned.}

]

Each type has a default signedness, endianness, and width in bits, as
described above. These can all be overridden individually:

@itemize[

  @item{@racket[:unsigned] and @racket[:signed] specify that integers
  should be decoded in an unsigned or signed manner, respectively.}

  @item{@racket[:big-endian], @racket[:little-endian] and
  @racket[:native-endian] specify the endianness to use in decoding
  integers or floats. Specifying @racket[:native-endian] causes Racket
  to use whatever is the native endianness of the platform the program
  is currently running on (discovered using
  @racket[system-big-endian?]).}

  @item{@racket[:default] causes the decoder to use whatever the
  default width is for the type specified.}

  @item{@racket[:bytes integer] causes the decoder to try to consume
  @racket[integer] bytes of input for this segment-pattern.}

  @item{@racket[:bits integer] causes the decoder to try to consume
  @racket[integer] bits of input for this segment-pattern.}

]

For example:

@racketblock[
	     (bit-string-case some-input-value
	       ([(= 0 :bytes 2)] 'a)
	       ([(? f :bits 10)
		 (:discard :binary)]
		(when (and (< f 123) (>= f 100)))
		'between-100-and-123)
	       ([(? f :bits 10)
		 (:discard :bits 6)]
		f)
	       ([(? f :bits 10)
		 (:discard :bits 6)
		 (? rest :binary)]
		(list f rest))

	       )
]

This expression analyses @racket[some-input-value], which must be a
@racket[(bit-string?)]. It may contain:

@itemize[

  @item{16 zero bits, in which case the result is @racket['a];
  or}

  @item{a ten-bit big-endian unsigned integer followed by 6 bits which
  are ignored, where the integer is between 100 (inclusive) and
  123 (exclusive), in which case the result is
  @racket['between-100-and-123];
  or}

  @item{the same as the previous clause, but without the guard;
  if this succeeds, the result is the ten-bit integer itself;
  or}

  @item{the same as the previous clause, but with an arbitrary number
  of bits following the six discarded bits. The result here is a list
  containing the ten-bit integer and the trailing bit string.}

]

The following code block parses a Pascal-style byte string and decodes
it using a UTF-8 codec:

@racketblock[
	     (bit-string-case input-bit-string
	       ([(? len)
		 (? body :binary :bytes len)]
		(bytes->string/utf-8 (bit-string-pack body))))
]

Notice how the @racket[len] value, which came from the input bit
string itself, is used to decide how much of the remaining input to
consume.

}

@subsection{Assembling bit strings from pieces}

@defform/subs[#:literals (:binary :integer :float
			  :little-endian :big-endian :native-endian
			  :bytes :bits :default)
	      (bit-string spec ...)
	      ((spec [segment-expr maybe-type maybe-endianness maybe-width]
		     segment-expr)
	       (maybe-type code:blank
			   (code:line :integer)
			   (code:line :float)
			   (code:line :binary))
	       (maybe-endianness code:blank
				 (code:line :big-endian)
				 (code:line :little-endian)
				 (code:line :native-endian))
	       (maybe-width code:blank
			    (code:line :bytes integer)
			    (code:line :bits integer)
			    (code:line :default)))]{

This form assembles and encodes a collection of integer,
floating-point numbers, and sub-bit-strings into a single bit
string. Each of the zero or more @racket[spec]s supplies zero or more
bits of the resulting bit string.

Each @racket[spec] can specify an integer or floating-point number to
encode, or a bit string to copy into the output. If a type is not
specified, @racket[:integer] is assumed. If an endianness is (relevant
but) not specified, @racket[:big-endian] is assumed. If a width is not
given, @racket[:integer]s are encoded as 8-bit quantities,
@racket[:float]s are encoded as 64-bit quantities, and
@racket[:binary] objects are copied into the output in their entirety.

If a width is specified, integers will be truncated or sign-extended
to fit, and binaries will be truncated. If a binary is shorter than a
specified width, an error is signalled. Floating-point encoding can
only be done using 32- or 64-bit widths.

For example:

@racketblock[
	     (define (string->pascal/utf-8 str)
	       (let ((bs (string->bytes/utf-8 str)))
		 (bit-string [(bytes-length bs)]
			     [bs :binary])))
]

This subroutine encodes its string argument using a UTF-8 codec, and
then assembles it into a Pascal-style string with a prefix length
byte. If the encoded string is longer than 255 bytes, note that the
length byte will be truncated and so the encoding will be incorrect. A
better encoder would ensure that @racket[bs] was not longer than 255
bytes before encoding it as a Pascal string.

Note that if you wish to leave all the modifiers at their
defaults (that is, @racket[:integer :bits 8]), and the expression you
want to encode is held in a variable, you can use the second form of
@racket[spec] given above: that is, you can simply mention the
variable. For example, the above subroutine could also have been
written as follows:

@racketblock[
	     (define (string->pascal/utf-8 str)
	       (let* ((bs (string->bytes/utf-8 str))
		      (len (bytes-length bs)))
		 (bit-string len [bs :binary])))
]

}

@subsection{Bit string utilities}

