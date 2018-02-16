all: build fix-highlighting export

build:
	rm -rf book/
	rm -rf docs/*
	mdbook build

fix-highlighting:
	cp static/highlight-js/highlight.pack.js book/highlight.js

export:
	cp book/* docs/ -r
