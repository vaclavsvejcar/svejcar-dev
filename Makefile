.PHONY: build
build: hlint headroom pretty
	stack build
	stack haddock

.PHONY: clean
clean:
	rm -rf ./.stack-work/
	rm -rf ./dist-newstyle

.PHONY: pretty
pretty: 
	find ./src -name '*.hs' | xargs stylish-haskell -i -v
	find ./src -name '*.hs' | xargs brittany --write-mode=inplace

.PHONY: fresh
fresh: clean build

.PHONY: headroom
headroom:
	headroom run -c

.PHONY: hlint
hlint:
	hlint ./src
