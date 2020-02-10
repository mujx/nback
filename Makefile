nix-shell:
	nix-shell --attr env release.nix

test:
	nix-shell --attr env release.nix --run "cabal test"

release:
	nix-build release.nix

install:
	nix-env -f ./release.nix -i

gen-nix:
	cabal2nix --no-check --no-haddock . > default.nix

clean:
	git clean -xdf

hlint:
	@hlint app && hlint ./test && hlint src

fmt:
	ormolu -m inplace ./**/*.hs

.PHONY: test
