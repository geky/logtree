TARGET = rbydtree
MAIN = Main.hs
SRC = $(wildcard *.hs)

GHC ?= ghc

override GHCFLAGS += -W
override GHCFLAGS += -O1
#override GHCFLAGS += -O2
override GHCFLAGS += -rtsopts
override GHCFLAGS += -fspecialise-aggressively
override GHCFLAGS += -fexpose-all-unfoldings
override GHCFLAGS += -Wno-unsupported-llvm-version
override GHCFLAGS += -Wincomplete-record-updates
override GHCFLAGS += -Wincomplete-uni-patterns
override GHCFLAGS += -Wcompat
override GHCFLAGS += -Wredundant-constraints

.PHONY: all build
all build: $(TARGET)

# let ghc decide if rebuild
.PHONY: $(TARGET)
$(TARGET): $(SRC)
	$(GHC) $(GHCFLAGS) $(MAIN) -o $(TARGET)

.PHONY: run
run:
	./$(TARGET) +RTS -M100m

.PHONY: clean
clean:
	rm -f $(TARGET)
	rm -f $(SRC:.hs=.hi) $(SRC:.hs=.o)
	
