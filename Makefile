COBC=cobc 
COBCQUIET=-Wall
COBCFLAGS=-Wall -fixed -I copybooks
#-Wextra
COBCJOB=-jx $(COBCFLAGS)
COBCBLD=-x $(COBCFLAGS)
COBCBLQ=-x $(COBCQUIET)
PROJ=shopcart
# MUCK=muck
SRC=$(PROJ).cob 
# MUCKSRC=$(MUCK).cob
CLEANUP=shopcart product.dat

# Build everything LOUDLY
all: clean $(PROJ)

# Build the code base, but only show basic warnings
quiet: $(SRC)
	$(COBC) $(COBCBLQ) $(SRC)

# Build and run the code
run: clean $(SRC)
	$(COBC) $(COBCJOB) $(SRC)

# Just build the code
$(PROJ): $(SRC)
	$(COBC) $(COBCBLD) $(SRC)

# Practice code to test ideas
# $(MUCK): $(MUCKSRC)
# # 	rm $(MUCK)
# 	$(COBC) $(COBCFLAGS) $(MUCKSRC)

# Remove the executable
clean:
	echo "Removing $(CLEANUP)"
	rm -rf $(CLEANUP) *.dat
	