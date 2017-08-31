
SRC=node
INST_DIR=../../cil-inst

#obj/$(SRC): objDirCheck obj/$(SRC).o 
	#gcc obj/$(SRC).o -o obj/$(SRC)

#obj/$(SRC).o: objDirCheck $(SRC).c
	#gcc $(SRC).c -c -o obj/$(SRC).o

obj/$(SRC): objDirCheck obj/$(SRC).o
	gcc obj/$(SRC).o obj/args.o obj/par-alloc.o obj/plain.o -o obj/$(SRC) -lm

obj/$(SRC).o: objDirCheck $(SRC).c args.c par-alloc.c plain.c
	gcc args.c  -c -o obj/args.o
	gcc plain.c  -c -o obj/plain.o
	gcc par-alloc.c  -c -o obj/par-alloc.o
	gcc $(SRC).c -c -o obj/$(SRC).o
	
objDirCheck:
	mkdir -p obj

run: obj/$(SRC)
	./obj/$(SRC) 4 1
	#sed -i "\$$a</events>" trace.xml
	#sed -i "s/&/&amp;/g" trace.xml
	#nm -n ./obj/$(SRC) > addresses
	
clean:
	rm -rf obj
	rm -f trace.xml types.xml $(SRC).cil.c *matrix addresses *xml

checkxml:
	xmllint --schema ../../xml/trace-schema.xsd --noout trace.xml 

all: run checkxml
