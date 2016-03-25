SRC = src/LinearAlgebra/*.elm

build:
	elm make $(SRC)

buildtest:
	(cd test && elm make Test.elm)

watch:
	$(MAKE) buildtest
	fswatch -l 0.2 $(SRC) test/Test.elm | \
	  while read f; do echo "$$f"; $(MAKE) build buildtest; done

testbrowser:
	browser-sync start --server --files test/index.html --startPath test/index.html
