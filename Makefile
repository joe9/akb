
# got this idea from
# https://github.com/Tuplanolla/ld-prehaskell/blob/master/makefile

CC=gcc
TMPDIR=/tmp/ghc/
GHCVERSION=`TMPDIR=/tmp/ghc stack exec ghc -- --numeric-version`

all: src/libskb.so src/libskb-xkbcommon.so

clean:
	$(RM) $(TMPDIR)/build/*.o $(TMPDIR)/build/*.hi src/libskb.so src/Skb_stub.h src/KeySymbolDefinitions.hs src/skb-libxkbcommon.so

src/libskb.so: src/**/*.hs src/skb.c src/skb.h
	test -d $(TMPDIR) || mkdir $(TMPDIR)
	test -d $(TMPDIR)/build || mkdir $(TMPDIR)/build
	TMPDIR=$(TMPDIR) stack build
	cd src && hsc2hs KeySymbolDefinitions.hsc
	cd src && \
		TMPDIR=$(TMPDIR) stack exec ghc -- --make \
		    -odir $(TMPDIR)/build/ \
		    -hidir $(TMPDIR)/build/ \
		    -O2 -dynamic -shared -fPIC \
		    -o libskb.so Skb.hs skb.c \
		    -l"HSrts-ghc$(GHCVERSION)"

src/libskb-xkbcommon.so: src/**/*.hs src/xkb.c src/skb.h
	test -d $(TMPDIR) || mkdir $(TMPDIR)
	test -d $(TMPDIR)/build || mkdir $(TMPDIR)/build
	TMPDIR=$(TMPDIR) stack build
	cd src && hsc2hs KeySymbolDefinitions.hsc
	cd src && \
		TMPDIR=$(TMPDIR) stack exec ghc -- --make \
		    -odir $(TMPDIR)/build/ \
		    -hidir $(TMPDIR)/build/ \
		    -O2 -dynamic -shared -fPIC \
		    -o libskb-xkbcommon.so Xkb.hs xkb.c skb.c \
		    -l"HSrts-ghc$(GHCVERSION)"

# got this idea from
#  http://stackoverflow.com/questions/10858261/abort-makefile-if-variable-not-set
install: all
ifndef WLD
	$(error WLD is not set)
endif
	test -d $(WLD)/lib/ || mkdir $(WLD)/lib/
	test -d $(WLD)/include/ || mkdir $(WLD)/include/
	cp src/libskb.so $(WLD)/lib/
	cp src/libskb-xkbcommon.so $(WLD)/lib/
	cp src/Skb_stub.h $(WLD)/include/
	cp src/skb.h $(WLD)/include/

.PHONY: all clean install
