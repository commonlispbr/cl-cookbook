export:
	rm -rf book/
	rm -rf docs/*
	mdbook build
	cp book/* docs/ -r
