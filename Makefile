.PHONY: build
build: format
	stack build
	stack haddock

.PHONY: clean
clean:
	rm -rf ./.stack-work/
	rm -rf ./dist-newstyle

.PHONY: format
format: 
	find ./src -name '*.hs' | xargs stylish-haskell -i -v
	find ./src -name '*.hs' | xargs brittany --write-mode=inplace

.PHONY: fresh
fresh: clean build
