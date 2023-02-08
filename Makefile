# make <\x00|run|clean|test>

EXEC = MiniHaskell
TEST_DIR = ./inputs
MAIN_TEST = $(TEST_DIR)/input.txt

run:
	ghc --make Main.hs -o $(EXEC) -package mtl
	./$(EXEC) < $(MAIN_TEST)

all:
	ghc --make Main.hs -package mtl

clean:
	rm -f *.hi *.o $(EXEC)
