PACKAGENAME=bitsyntax
COLLECTS=bitsyntax

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf htmldocs

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test: setup testonly

testonly:
	raco test -p $(PACKAGENAME)

htmldocs:
	raco scribble \
		--html \
		--dest htmldocs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		bitsyntax/scribblings/bitsyntax.scrbl

pages:
	@(git branch -v | grep -q gh-pages || (echo local gh-pages branch missing; false))
	@echo
	@git branch -av | grep gh-pages
	@echo
	@(echo 'Is the branch up to date? Press enter to continue.'; read dummy)
	git clone -b gh-pages . pages

publish: htmldocs pages
	rm -rf pages/*
	cp -r htmldocs/. pages/.
	(cd pages; git add -A)
	-(cd pages; git commit -m "Update $$(date +%Y%m%d%H%M%S)")
	(cd pages; git push)
	rm -rf pages
