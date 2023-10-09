# make <\x00|run|clean|test>

EXEC = MiniHaskell
TEST_DIR = ../inputs
MAIN_TEST = $(TEST_DIR)/input_fg_comp.txt

CANDY = ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I was bored
define eye_candy
	$(info )
	$(info $(CANDY) $(1) $(CANDY) )
	$(info )
endef

# Basic
all:
	$(call eye_candy, BUILDING)
	ghc --make Main.hs -o $(EXEC) -package mtl

# Test, test
run:
	$(call eye_candy, RUNNING)
	ghc --make Main.hs -o $(EXEC) -package mtl
	./$(EXEC) < $(MAIN_TEST) || make clean

# Latza
clean:
	$(call eye_candy, CLEANING)
	rm -f *.hi *.o $(EXEC)

deliver:
	git archive --format=zip --output Pantazis_Harry_1115201800218.zip master
