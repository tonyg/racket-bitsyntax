#lang scribble/manual

@(require planet/scribble
	  scribble/racket
	  (for-label racket
		     (this-package-in main)))

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
			  = :
			  binary integer float
			  signed unsigned
			  little-endian big-endian native-endian
			  bytes bits default)
	      (bit-string-case value-expr clause ...)
	      ((clause ([segment-pattern ...]
			(when guard-expr)
			body-expr ...)
		       ([segment-pattern ...]
			body-expr ...)
		       (else
			body-expr ...))
	       (segment-pattern comparison-pattern
				binding-pattern
				discard-pattern)
	       (comparison-pattern (= expr : option ...)
				   (= expr))
	       (binding-pattern (id : option ...)
				(id)
				id)
	       (discard-pattern (: option ...))
	       (option type-option
		       signedness-option
		       endianness-option
		       width-option)
	       (type-option integer
			    float
			    binary)
	       (signedness-option unsigned
				  signed)
	       (endianness-option little-endian
				  big-endian
				  native-endian)
	       (width-option (code:line bits n)
			     (code:line bytes n)
			     default))]{

The @racket[value-expr] is evaluated first. It must evaluate to a bit
string---any object for which @racket[bit-string?] would return
@racket[#t].

Each @racket[clause] is then tried in turn. The first succeeding
clause determines the result of the whole expression. A clause matches
successfully if all its @racket[segment-pattern]s match some portion
of the input, there is no unused input left over at the end, and the
@racket[guard-expr] (if there is one) evaluates to a true value. If a
@racket[clause] succeeds, then @racket[(begin body-expr ...)] is
evaluated, and its result becomes the result of the whole expression.

If none of the @racket[clause]s succeed, and there is an @racket[else]
clause, its @racket[body-expr]s are evaluated and returned. If there's
no @racket[else] clause and none of the others succeed, an error is
signalled.

Each @racket[segment-pattern] matches zero or more @italic{bits} of
the input bit string. The given type, signedness, endianness and width
are used to extract a value from the bit string, at which point it is
either compared to some other value using @racket[equal?] (if a
@racket[comparison-pattern] was used in the segment-pattern), bound to
a pattern variable (if a @racket[binding-pattern] was used), or
discarded (if a @racket[discard-pattern] was used) before matching
continues with the next @racket[segment-pattern].

The supported segment types are

@itemize[

  @item{@racket[integer] -- this is the default. A signed or unsigned,
  big- or little-endian integer of the given width in bits is read out
  of the bit string. Unless otherwise specified, integers default to
  big-endian, unsigned, and @bold{eight bits wide}. Any width, not just
  multiples of eight, is supported.}

  @item{@racket[float] -- A 32- or 64-bit float in either big- or
  little-endian byte order is read out of the bit string using
  @racket[floating-point-bytes->real]. Unless otherwise specified,
  floats default to big-endian and 64 bits wide. Widths other than 32 or
  64 bits are unsupported.}

  @item{@racket[binary] -- A sub-bit-string is read out of the bit
  string. The bit string can be an arbitrary number of bits long, not
  just a multiple of eight. Unless otherwise specified, the entire rest
  of the input will be consumed and returned.}

]

Each type has a default signedness, endianness, and width in bits, as
described above. These can all be overridden individually:

@itemize[

  @item{@racket[unsigned] and @racket[signed] specify that integers
  should be decoded in an unsigned or signed manner, respectively.}

  @item{@racket[big-endian], @racket[little-endian] and
  @racket[native-endian] specify the endianness to use in decoding
  integers or floats. Specifying @racket[native-endian] causes Racket
  to use whatever is the native endianness of the platform the program
  is currently running on (discovered using
  @racket[system-big-endian?]).}

  @item{@racket[default] causes the decoder to use whatever the
  default width is for the type specified.}

  @item{@racket[bytes n] causes the decoder to try to consume
  @racket[n] bytes of input for this segment-pattern.}

  @item{@racket[bits n] causes the decoder to try to consume
  @racket[n] bits of input for this segment-pattern.}

]

For example:

@racketblock[
	     (bit-string-case some-input-value
	       ([(= 0 : bytes 2)] 'a)
	       ([(f : bits 10) (: binary)]
		(when (and (< f 123) (>= f 100)))
		'between-100-and-123)
	       ([(f : bits 10) (: bits 6)]
		f)
	       ([(f : bits 10) (: bits 6) (rest : binary)]
		(list f rest)))
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

The following code block parses a Pascal-style byte string (one length
byte, followed by the right number of data bytes) and decodes it using
a UTF-8 codec:

@racketblock[
	     (bit-string-case input-bit-string
	       ([len (body : binary bytes len)]
		(bytes->string/utf-8 (bit-string-pack body))))
]

Notice how the @racket[len] value, which came from the input bit
string itself, is used to decide how much of the remaining input to
consume.

}

@subsection{Assembling bit strings from pieces}

@defform/subs[#:literals (:
			  binary integer float
			  little-endian big-endian native-endian
			  bytes bits default)
	      (bit-string spec ...)
	      ((spec [segment-expr : option ...]
		     segment-expr)
	       (option type-option
		       endianness-option
		       width-option)
	       (type-option integer
			    float
			    binary)
	       (endianness-option little-endian
				  big-endian
				  native-endian)
	       (width-option (code:line bits n)
			     (code:line bytes n)
			     default))]{

This form assembles and encodes a collection of integer,
floating-point numbers, and/or sub-bit-strings into a single bit
string. Each of the zero or more @racket[spec]s supplies zero or more
bits of the resulting bit string.

Each @racket[spec] can specify an integer or floating-point number to
encode, or a bit string to copy into the output. If a type is not
specified, @racket[integer] is assumed. If an endianness is (relevant
but) not specified, @racket[big-endian] is assumed. If a width is not
given, @racket[integer]s are encoded as 8-bit quantities,
@racket[float]s are encoded as 64-bit quantities, and @racket[binary]
objects are copied into the output in their entirety.

If a width is specified, integers will be truncated or sign-extended
to fit, and binaries will be truncated. If a binary is shorter than a
specified width, an error is signalled. Floating-point encoding can
only be done using 32- or 64-bit widths.

For example:

@racketblock[
	     (define (string->pascal/utf-8 str)
	       (let ((bs (string->bytes/utf-8 str)))
		 (bit-string (bytes-length bs) [bs : binary])))
]

This subroutine encodes its string argument using a UTF-8 codec, and
then assembles it into a Pascal-style string with a prefix length
byte. If the encoded string is longer than 255 bytes, note that the
length byte will be truncated and so the encoding will be incorrect. A
better encoder would ensure that @racket[bs] was not longer than 255
bytes before encoding it as a Pascal string.

Note that if you wish to leave all the options at their defaults (that
is, @racket[[... : integer bits 8]]), you can use the second form of
@racket[spec] given above.

}

@subsection{Bit string utilities}

@defproc[(bit-string? [x any?]) boolean?]{
Returns @racket[#t] if its argument is either a @racket[bytes?], a
@racket[bit-slice?] or a @racket[splice?]. Returns @racket[#f]
otherwise.}

@defproc[(bit-string-length [x bit-string?]) integer?]{
Returns the length of its argument, in bits.}

@defproc[(bit-string-empty? [x bit-string?]) boolean?]{
Returns @racket[#t] if its argument's @racket[bit-string-length] is
zero.}

@defproc[(bit-string-append [a bit-string?]
			    [b bit-string?]) bit-string?]{
Appends its arguments, producing a new bit string. Uses
@racket[splice] internally when it can't arrange to return a bit
string previously constructed. (The practical upshot of this is that
you might need to use @racket[bit-string->bytes] to "flatten" appended
bit-strings from time to time.)}

@defproc[(bit-string-split-at [x bit-string?]
			      [offset integer?]) (values bit-string? bit-string?)]{

Produces two values: the bit-string containing bits
[@racket[0]..@racket[offset]) of @racket[x], and the bit-string
containing bits [@racket[offset]..@racket[(bit-string-length x)]) of
@racket[x]. If offset is negative or greater-or-equal-to the number of
bits in @racket[x], an error is signalled.}

@defproc[(bit-string-split-at-or-false [x bit-string?]
				       [offset integer?])
         (values (or/c bit-string? #f) (or/c bit-string? #f))]{

Like @racket[(bit-string-split-at x offset)], but if @racket[offset]
is out of range returns @racket[(values #f #f)] instead of signalling
an error. This procedure is used in the implementation of
@racket[bit-string-case].}

@defproc[(sub-bit-string [x bit-string?]
			 [low-bit integer?]
			 [high-bit integer?]) bit-string?]{

If @racket[(<= 0 low-bit high-bit (sub1 (bit-string-length x)))],
returns the bit-string containing bits
[@racket[low-bit]..@racket[high-bit]) of @racket[x]. Otherwise,
signals an error.}

@defproc[(bit-string-ref [x bit-string?] [offset integer?]) (or/c 0 1)]{

Extracts bit number @racket[offset] from @racket[x]. Signals an error
if @racket[offset] is negative or greater-than-or-equal-to the length
of @racket[x].}

@defproc[(bit-string->bytes [x bit-string?]) bytes?]{

Flattens any splices or bit-slices in @racket[x], producing a single
contiguous byte vector with @racket[x]'s contents. If
@racket[(positive? (remainder (bit-string-length x) 8))], pads out the
remaining bits with zeros on the right.}

@defproc[(bit-string->bytes/align [x bit-string?] [align-right? boolean?]) bytes?]{

As @racket[bit-string->bytes], but offers the choice of padding on the
right (if @racket[align-right?] is @racket[#f]) or on the left (if
@racket[align-right?] is @racket[#t]) when padding is required. (Note
that to align the bits in @racket[x] on the right is to pad with zeros
on the left, and vice versa.)}

@defproc[(bit-string-byte-count [x bit-string?]) integer?]{

Returns the smallest number of whole bytes that could contain all the
bits in @racket[x].}

@defproc[(bit-string-pack! [x bit-string?]
			   [buf bytes?]
			   [offset integer?]) void?]{

Copies the entirety of @racket[x] into @racket[buf], overwriting bits
starting with the @racket[offset]th. It is an error for @racket[buf]
not to have enough room or for @racket[offset] to be out-of-bounds.}

@defproc[(bit-string-pack [x bit-string?]) bit-string?]{

Returns a bit string equivalent to @racket[x] (i.e. with exactly the
same bits in the same order) but with any intermediate splices or
bit-slices flattened away. The result will either be a @racket[bytes?]
of the correct length, or a @racket[bit-slice] referring to a section
of a byte vector of length @racket[(bit-string-byte-count x)].}

@defproc[(copy-bits! [target bit-string?]
		     [target-offset integer?]
		     [source bit-string?]
		     [source-offset integer?]
		     [count integer?]) void?]{

Overwrites bits [@racket[target-offset]..@racket[(+ target-offset
count)]) of @racket[target] with bits
[@racket[source-offset]..@racket[(+ source-offset count)]) of
@racket[source]. Undefined behaviour results when @racket[(eq? target
source)].}

@defproc[(bit-string->integer [x bit-string?]
			      [big-endian? boolean?]
			      [signed? boolean?]) integer?]{

Interprets the bits in @racket[x] as an integer, using either a big-
or little-endian byte-ordering convention (per @racket[big-endian?]),
and either unsigned or two's-complement signed arithmetic (per
@racket[signed?]) to produce the result.}

@defproc[(integer->bit-string [n integer?]
			      [width integer?]
			      [big-endian? boolean?]) bit-string?]{

Encodes @racket[n] as a bit string of length @racket[width] bits,
truncating or sign-extending as required, and using a big- or
little-endian byte-ordering convention as per @racket[big-endian?].}

@subsection{Debugging utilities}

These procedures may be useful for debugging, but should not be relied
upon otherwise.

@defproc[(bit-slice? [x any?]) boolean?]{

Returns @racket[#t] if and only if @racket[x] is a bit-slice.}

@defproc[(bit-slice-binary [x bit-slice?]) bytes?]{

Extracts the underlying byte vector from a bit-slice.}

@defproc[(bit-slice-low-bit [x bit-slice?]) integer?]
@defproc[(bit-slice-high-bit [x bit-slice?]) integer?]{

Extract the low (inclusive) and high (exclusive) bit indexes,
respectively, from a bit-slice. The bit-slice itself represents bits
[@racket[low]..@racket[high]) of the underlying byte vector.}

@defproc[(splice? [x any?]) boolean?]{

Returns @racket[#t] if and only if @racket[x] is a splice of two
bit-strings.}

@defproc[(splice-left [x splice?]) bit-string?]
@defproc[(splice-right [x splice?]) bit-string?]{

Extract the left and right bit-strings, respectively, that are spliced
together by the given splice @racket[x].}
