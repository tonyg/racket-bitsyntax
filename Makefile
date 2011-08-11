PLANET_VERSION=3.2

all:

bitsyntax.plt: clean
	mkdir planet-build-temp
	(cd planet-build-temp; git clone .. bitsyntax)
	(cd planet-build-temp/bitsyntax; git checkout bitsyntax.plt-${PLANET_VERSION})
	(cd planet-build-temp; raco planet create bitsyntax)
	mv planet-build-temp/bitsyntax.plt .
	rm -rf planet-build-temp

manual.html: manual.scrbl
	raco scribble $<

clean:
	rm -f manual.html racket.css scribble-common.js scribble-style.css scribble.css
	rm -rf planet-docs
	rm -f bitsyntax.plt
