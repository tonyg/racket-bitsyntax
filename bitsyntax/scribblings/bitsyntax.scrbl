#lang scribble/manual

@(require scribble/racket
	  (for-label racket
		     bitsyntax))

@title[#:version "4.0"]{bitsyntax}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@section{Introduction}

This library adds three features to Racket:

@itemize[
  @item{library support for @italic{bit strings}, a generalization of
  byte vectors;
  }

  @item{syntactic support for extracting integers, floats,
  sub-bit-strings and general values from bit strings;
  and}

  @item{syntactic support for constructing bit strings from integers,
  floats, other bit strings and general values.}
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

The binary matching (@racket[bit-string-case]) and formatting
(@racket[bit-string]) languages can also be extended with custom
parsers and formatters, giving lightweight syntactic support for
domain-specific binary encodings of values.

If you find that this library lacks some feature you need, or you have
a suggestion for improving it, please don't hesitate to
@link["mailto:tonygarnockjones@gmail.com"]{get in touch with me}!

@section{Changes}

Version 4.0 of this library changes the way custom parsers and
formatters work, requiring them to be macros, where in previous
releases they could have been implemented as functions. Version 4.0 of
the library also supports the use of @racket[bit-string-case] and
@racket[bit-string] in Typed Racket code.

Version 4.0 is also the final release made using the Planet package
system. Current and future versions of this package will use the new
Racket package system ("@tt{raco pkg}") instead.

Version 3.2 of this library adds support for custom parsers and
formatters.

Version 3.0 of this library uses @racket[::] instead of @racket[:] to
separate expressions from encoding specifications in the
@racket[bit-string-case] and @racket[bit-string] macros. The reason
for this is to avoid a collision with Typed Racket, which uses
@racket[:] for its own purposes.

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

@defmodule[bitsyntax]

@subsection{Types}

@defidform[BitString]{The Typed Racket type of bit strings. A value
has type @racket[BitString] if and only if it also results in
@racket[#t] when given to @racket[bit-string?].}

@subsection{Pattern-matching bit strings}

@defform/subs[#:literals (when else
			  = ::
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
	       (comparison-pattern (= expr :: custom-parser)
				   (= expr :: option ...)
				   (= expr))
	       (binding-pattern (id :: custom-parser)
				(id :: option ...)
				(id)
				id)
	       (discard-pattern (:: custom-parser)
				(:: option ...))
	       (custom-parser (expr expr ...))
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

  @item{@racket[custom-parser] -- An arbitrary amount of the input is
  consumed, analysed, and transformed. The built-in parsing options
  can't be specified in conjunction with a custom parser: instead, each
  custom parser accepts its own option arguments. See
  @secref{custom-parsers}.}

]

Each type (except for @racket[custom-parser]) has a default
signedness, endianness, and width in bits, as described above. These
can all (again, except for @racket[custom-parser]s, which manage such
issues on their own) be overridden individually:

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
	       ([(= 0 :: bytes 2)] 'a)
	       ([(f :: bits 10) (:: binary)]
		(when (and (< f 123) (>= f 100)))
		'between-100-and-123)
	       ([(f :: bits 10) (:: bits 6)]
		f)
	       ([(f :: bits 10) (:: bits 6) (rest :: binary)]
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
	       ([len (body :: binary bytes len)]
		(bytes->string/utf-8 (bit-string-pack body))))
]

Notice how the @racket[len] value, which came from the input bit
string itself, is used to decide how much of the remaining input to
consume.

}

@subsection{Assembling bit strings from pieces}

@defform/subs[#:literals (::
			  binary integer float
			  little-endian big-endian native-endian
			  bytes bits default)
	      (bit-string spec ...)
	      ((spec [segment-expr :: custom-formatter]
		     [segment-expr :: option ...]
		     segment-expr)
	       (custom-formatter (expr expr ...))
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

This form assembles and encodes a collection of values into a single
bit string. Each of the zero or more @racket[spec]s supplies zero or
more bits of the resulting bit string. The core language supports
encoding of integers, floating-point numbers, and bit-strings, and
custom formatters (see @secref{custom-parsers}) can be used to give a
convenient syntax for encoding other kinds of value.

Each @racket[spec] can specify an integer or floating-point number to
encode, a bit string to copy into the output, or a custom formatting
routine to apply to the given value. If a type is not specified,
@racket[integer] is assumed. If an endianness is (relevant but) not
specified, @racket[big-endian] is assumed. If a width is not given,
@racket[integer]s are encoded as 8-bit quantities, @racket[float]s are
encoded as 64-bit quantities, and @racket[binary] objects are copied
into the output in their entirety. Custom formatters do not accept
these options, since they manage the encoding of the value they are
given themselves, and take whatever options they need by other means.

If a width is specified, integers will be truncated or sign-extended
to fit, and binaries will be truncated. If a binary is shorter than a
specified width, an error is signalled. Floating-point encoding can
only be done using 32- or 64-bit widths.

For example:

@racketblock[
	     (define (string->pascal/utf-8 str)
	       (let ((bs (string->bytes/utf-8 str)))
		 (bit-string (bytes-length bs) [bs :: binary])))
]

This subroutine encodes its string argument using a UTF-8 codec, and
then assembles it into a Pascal-style string with a prefix length
byte. If the encoded string is longer than 255 bytes, note that the
length byte will be truncated and so the encoding will be incorrect. A
better encoder would ensure that @racket[bs] was not longer than 255
bytes before encoding it as a Pascal string.

Note that if you wish to leave all the options at their defaults (that
is, @racket[[... :: integer bits 8]]), you can use the second form of
@racket[spec] given above.

}

@subsection[#:tag "custom-parsers"]{Custom parsers and custom formatters}

For simple uses of @racket[bit-string-case] and @racket[bit-string],
the built-in parsers and formatters will often be enough. Many binary
data formats, however, make heavy use of domain-specific value
encodings, and it quickly becomes either repetitive or awkward and
error-prone to express these domain-specific formats. Custom parsers
and custom formatters exist to allow you to extend both
@racket[bit-string-case] and @racket[bit-string] to provide convenient
shortcut syntax for domain-specific data formats.

For example, imagine a particular protocol makes heavy use of
Pascal-style strings: sequences of UTF-8 encoded bytes prefixed by a
single length byte, intrinsically limited to a maximum length of 255
bytes. Performing the necessary checks and transformations quickly
gets repetitive, as you can see:

@racketblock[
	     (bit-string-case packet
	       ([(= PACKET-TYPE-ONE)
		 username-length (raw-username :: binary bytes username-length)
		 password-length (raw-password :: binary bytes password-length)]
		(let ((username (bytes->string/utf-8 raw-username))
		      (password (bytes->string/utf-8 raw-password)))
		  ...))
	       ([(= PACKET-TYPE-TWO)
		 (error-code :: big-endian integer bytes 2)
		 error-text-length
		 (raw-error-text :: binary bytes error-text-length)]
		(let ((error-text (bytes->string/utf-8 raw-error-text)))
		  ...))
	       ...)
]

On the formatting side, things are just as bad:

@racketblock[
	     (define (encode-packet-type-one username password)
	       (let ((raw-username (string->bytes/utf-8 username))
		     (raw-password (string->bytes/utf-8 password)))
		 (when (> (bytes-length raw-username) 255)
		   (error 'encode-packet-type-one "Username too long"))
		 (when (> (bytes-length raw-password) 255)
		   (error 'encode-packet-type-one "Password too long"))
		 (bit-string PACKET-TYPE-ONE
			     (bytes-length raw-username)
			     (raw-username :: binary)
			     (bytes-length raw-password)
			     (raw-password :: binary))))
]

By introducing a custom extension, comprising both a parser and
formatter together, we can improve the situation enormously:

@racketblock[
	     (define-syntax pascal-string/utf-8
	       (syntax-rules ()
		 [(_ #t input ks kf)
		  (code:comment "The first argument to the custom parser/formatter")
		  (code:comment "will be a literal #t to signal it is being used")
		  (code:comment "as a parser.")
		  (bit-string-case input
		    ([len (body :: binary bytes len) (rest :: binary)]
		     (ks (bytes->string/utf-8 (bit-string->bytes body)) rest))
		    (else
		     (kf)))]
		 [(_ #f str)
		  (code:comment "The first argument to the custom parser/formatter")
		  (code:comment "will be a literal #f to signal it is being used")
		  (code:comment "as a formatter.")
		  (let* ((bs (string->bytes/utf-8 str))
			 (len (bytes-length bs)))
		    (when (> len 255)
		      (error 'pascal-string/utf-8
			     "String of length ~v too long; max is 255 bytes"
			     len))
		    (bit-string len (bs :: binary)))]))
]

This single definition can now be used in any @racket[bit-string-case]
or @racket[bit-string] expression where it is in scope. Here's the
earlier example, rewritten to use @racket[pascal-string/utf-8]:

@racketblock[
	     (bit-string-case packet
	       ([(= PACKET-TYPE-ONE)
		 (username :: (pascal-string/utf-8))
		 (password :: (pascal-string/utf-8))]
		...)
	       ([(= PACKET-TYPE-TWO)
		 (error-code :: big-endian integer bytes 2)
		 (error-text :: (pascal-string/utf-8))]
		...)
	       ...)
]

Formatting is likewise much simplified:

@racketblock[
	     (define (encode-packet-type-one username password)
	       (bit-string PACKET-TYPE-ONE
			   (username :: (pascal-string/utf-8))
			   (password :: (pascal-string/utf-8))))
]

@subsubsection{Supplying arguments to custom parsers and formatters}

Custom parser/formatters must be macros that accept one or more
arguments. The first argument is a boolean flag, supplied as
@racket[#t] by @racket[bit-string-case] or as @racket[#f] by
@racket[bit-string], indicating whether the custom extension is being
used as a parser or a formatter, respectively. Following the flag,

@itemize[
  @item{parsers (@racket[#t]) are given the remaining @racket[input]
    to be parsed, a success continuation function @racket[ks] and a
    failure continuation function @racket[kf], and}
  @item{formatters (@racket[#f]) are given the @racket[value] to be
    formatted.}
]

Subsequent arguments are supplied by the programmer at each use of the
custom extension, and can be used to tweak the behaviour of the
extension on a case-by-case basis.

For example, let's suppose we didn't want to restrict ourselves to the
single length byte of Pascal-style strings, but wanted instead a more
flexible way of indicating that a certain block of bytes should be
interpreted and rendered as UTF-8 encoded text. We might define a
custom parser/formatter like the following:

@racketblock[
	     (define-syntax utf-8
	       (syntax-rules ()

		 (code:comment "Consume entirety of input, decode as UTF-8")
		 [(_ #t input ks kf)
		  (ks (bytes->string/utf-8 (bit-string->bytes input)) (bytes))]

		 (code:comment "Consume a prefix of the input, decode as UTF-8")
		 [(_ #t input ks kf length-in-bytes)
		  (bit-string-case input
		    ([ (body :: binary bytes length-in-bytes)
		       (rest :: binary) ]
		     (ks (bytes->string/utf-8 (bit-string->bytes body)) rest))
		    (else
		     (kf)))]

		 (code:comment "Encode the entire string without length prefix")
		 [(_ #f str)
		  (string->bytes/utf-8 str)]

		 (code:comment "Encode the entire string with a length prefix")
		 [(_ #f str (length-format-options ...))
		  (let* ((bs (string->bytes/utf-8 str))
			 (len (bytes-length bs)))
		    (bit-string (len :: length-format-options ...)
				(bs :: binary)))]))
]

@margin-note{A more general @racket[utf-8] would be able to specify a
length limit as well as a length format. Extending the example in this
way is left as an exercise for the reader.}

The @racket[utf-8] parser/formatter can then be used in any of four different ways:

@itemize[

  @item{In @racket[bit-string-case], as @racket[(var :: (utf-8))] --
  will take the remainder of the input and UTF-8 decode it to a string.}

  @item{In @racket[bit-string-case], as @racket[(var :: (utf-8 123))] --
  will take the next 123 bytes of the input and UTF-8 decode it to a
  string. Note that the length, here @racket[123], can come from some
  earlier field extracted from the input, leading to a form of dependent
  parsing.}

  @item{In @racket[bit-string], as @racket[(val :: (utf-8))] -- will
  encode and output the entirety of @racket[val] as UTF-8.}

  @item{In @racket[bit-string], as
  @racket[(val :: (utf-8 (option ...)))] -- will encode @racket[val]
  as UTF-8, and will prepend the length of the encoded text in the
  output. The length will be formatted using the @racket[option]s, along
  the lines of @racket[((bytes-length encoded-text) :: option ...)], so
  the length can be encoded in any way at all. A recursive use of a
  custom formatter could even encode it in a variable-length fashion.}

]

Giving arguments to custom parser/formatters opens the door to
utilities such as variable-length integer codecs, generic zlib-based
compressing codecs, generic encrypting codecs, generic transcoders and
so on.

Applications of a custom extension macro are rewritten by
@racket[bit-string-case] from @racket[(extension arg ...)] to
@racket[(extension #t input ks kf arg ...)], and by
@racket[bit-string] to @racket[(extension #f value arg ...)].

@subsubsection{The detailed anatomy of a custom extension}

A custom extension should accept

@itemize[
  @item{the boolean flag indicating whether it is being used as a parser or a formatter,}
  @item{additional arguments, depending on whether it is being used as a parser or a formatter, and}
  @item{any other arguments supplied at the time of use.}
]

When used in "parser" mode (with @racket[#t] as its first argument),
expects a piece of syntax denoting an input bit-string, a "success
continuation" and a "failure continuation" as its second through
fourth arguments. The result of expansion should analyse the input
bit-string as it sees fit. If it decides it has successfully matched a
prefix of the input, it should call the success continuation with two
arguments: the value extracted from the input prefix, and the
remaining unconsumed input (as a bit-string). If, on the other hand,
it decides it cannot match a prefix of the input, it should call the
failure continuation with no arguments.

When called in "formatter" mode (with @racket[#f] as its first
argument), it should expect a piece of syntax denoting the value to be
formatted as its second argument. The result of expansion should be an
expression resulting in the encoded form of this value, as a
bit-string.

The general form, then, of custom extensions, is:

@racketblock[
	     (define-syntax my-custom-extension
	       (syntax-rules ()
		 [(_ #t input success-k failure-k other-arg ...)
		  (if (analyze input)
		      (success-k result-of-analysis remainder-of-input)
		      (failure-k))]
		 [(_ #f value other-arg ...)
		  (format-value-as-bit-string value)]))
]

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

@defproc[(bit-string-append [a bit-string?] ...) bit-string?]{
Appends its arguments in order, producing a new bit string. Uses
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

@defproc[(copy-bits! [target bytes?]
		     [target-offset integer?]
		     [source bytes?]
		     [source-offset integer?]
		     [count integer?]) void?]{

Overwrites bits [@racket[target-offset]..@racket[(+ target-offset
count)]) of @racket[target] with bits
[@racket[source-offset]..@racket[(+ source-offset count)]) of
@racket[source]. Undefined behaviour results when @racket[(eq? target
source)].}

@defproc[(bit-string->integer [x bit-string?]
			      [big-endian? boolean?]
			      [signed? boolean?]) exact-integer?]{

Interprets the bits in @racket[x] as an integer, using either a big-
or little-endian byte-ordering convention (per @racket[big-endian?]),
and either unsigned or two's-complement signed arithmetic (per
@racket[signed?]) to produce the result.}

@defproc[(bit-string->byte [x bit-string?]) byte?]
@defproc[(bit-string->signed-integer [x bit-string?]
				     [big-endian? boolean?]) exact-integer?]
@defproc[(bit-string->unsigned-integer [x bit-string?]
				       [big-endian? boolean?]) exact-nonnegative-integer?]{

Specialized versions of @racket[bit-string->integer], giving better
type information for use with Typed Racket.

The function @racket[bit-string->byte] will raise an exception if
given a bit string of any length other than exactly eight bits.}

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
