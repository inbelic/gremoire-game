GHC=ghc
GHCI=ghci

ODIR=../build

_PACKAGES=base containers bytestring network
PACKAGES=$(patsubst %,$\-package %,$(_PACKAGES))

build:
	cd Gremoire; \
	$(GHC) --make -o $(ODIR)/gremoire Main -odir $(ODIR) \
	-hidir $(ODIR) $(PACKAGES)

debug: build
debug-run: build

clean:
	rm -rf build/*

.PHONY: build clean
