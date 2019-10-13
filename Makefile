nix-shell:
	nix-shell --attr env release.nix

release:
	nix-build release.nix

gen-nix:
	cabal2nix --no-check --no-haddock . > default.nix

clean:
	git clean -xdf

hlint:
	@hlint -q app && hlint -q ./test && hlint -q src

fmt:
	ormolu -m inplace ./**/*.hs

.PHONY: test
