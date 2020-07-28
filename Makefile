.PHONY: format
format: 
	find ./src -name '*.hs' | xargs stylish-haskell -i -v
	find ./src -name '*.hs' | xargs brittany --write-mode=inplace
