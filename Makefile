COBC=cobc 
COBCFLAGS=-x -Wall -Wextra
COBCJOB=-jx
PROJ=shopcart
SRC=$(PROJ).cob 

all: clean $(PROJ)

run: clean $(SRC)
	$(COBC) $(COBCJOB) $(SRC)

$(PROJ): $(SRC)
	$(COBC) $(COBCFLAGS) $(SRC)

clean:
	echo "Removing $(PROJ)"
	rm -rf $(PROJ)