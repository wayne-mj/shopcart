COBC=cobc 
COBCQUIET=-x -Wall
COBCFLAGS=-x -Wall -Wextra
COBCJOB=-jx
PROJ=shopcart
MUCK=muck
SRC=$(PROJ).cob 
MUCKSRC=$(MUCK).cob

# Build everything LOUDLY
all: clean $(PROJ)

# Build the code base, but only show basic warnings
quiet: $(SRC)
	$(COBC) $(COBCQUIET) $(SRC)

# Build and run the code
run: clean $(SRC)
	$(COBC) $(COBCJOB) $(SRC)

# Just build the code
$(PROJ): $(SRC)
	$(COBC) $(COBCFLAGS) $(SRC)

# Practice code to test ideas
$(MUCK): $(MUCKSRC)
	$(COBC) $(COBCFLAGS) $(MUCKSRC)

# Remove the executable
clean:
	echo "Removing $(PROJ)"
	rm -rf $(PROJ)