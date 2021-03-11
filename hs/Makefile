TARGET = logtree
MAIN = Main.hs
SRC = $(wildcard *.hs)

GHC ?= ghc

override GHCFLAGS += -W
override GHCFLAGS += -O2
override GHCFLAGS += -rtsopts
override GHCFLAGS += -fspecialise-aggressively
override GHCFLAGS += -fexpose-all-unfoldings
override GHCFLAGS += -Wno-unsupported-llvm-version

.PHONY: all build
all build: $(TARGET)

$(TARGET): $(SRC)
	$(GHC) $(GHCFLAGS) $(MAIN) -o $(TARGET)

.PHONY: run
run:
	./$(TARGET) +RTS -M100m

.PHONY: clean
clean:
	rm -f $(TARGET)
	rm -f $(SRC:.hs=.hi) $(SRC:.hs=.o)
	
