COBC=cobc 
COBCFLAGS=-x -Wall -Wextra
COBCJOB=-jx
PROJ=shopcart
SRC=$(PROJ).cob 

all: clean $(PROJ)

run: $(SRC)
	$(COBC) $(COBCJOB) $(SRC)

$(PROJ): $(SRC)
	$(COBC) $(COBCFLAGS) $(SRC)

clean:
	rm -rf $(PROJ)