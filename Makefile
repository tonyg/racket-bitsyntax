all:

manual.html: manual.scrbl
	raco scribble $<

clean:
	rm -f manual.html racket.css scribble-common.js scribble-style.css scribble.css
	rm -rf planet-docs
	rm -f bitsyntax.plt
