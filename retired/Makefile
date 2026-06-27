COBC=cobc
COPYBOOKS=copybooks
PROJ=shopcart
FLAGS=-x -I $(COPYBOOKS) -Wall
SRC=$(PROJ).cob
CLEANUP=shopcart *.dat *.txt

all: clean $(PROJ)

$(PROJ): $(SRC)
	$(COBC) $(FLAGS) $(SRC)

build-log: $(SRC)
	$(COBC) $(FLAGS) -X -T $(PROJ).txt $(SRC)

clean:
	rm -f $(CLEANUP)
