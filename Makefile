build:
	elm make src/Vector.elm

watch:
	$(MAKE) build
	fswatch -l 0.2 src/Vector.elm | \
	  while read f; do echo "$$f"; $(MAKE) build; done
