.PHONY: all
all: example.png input.png

%.svg: %.dot to-dot
	dot -Tsvg < $< > $@

%.png: %.dot to-dot
	dot -Tpng < $< > $@

%.dot: % to-dot
	./to-dot < $< > $@
