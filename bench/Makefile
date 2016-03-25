SRC = Bench.elm ../src/LinearAlgebra/*.elm

build:
	elm make Bench.elm

watch:
	-$(MAKE) build
	fswatch $(SRC) | \
	  while read f; do clear; echo "$$f"; $(MAKE) build; done

testbrowser:
	browser-sync start --server --files index.html --startPath index.html
