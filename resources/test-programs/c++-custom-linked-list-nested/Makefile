
SRC=c++-custom-linked-list-nested
INST_DIR=../../cil-inst

obj/$(SRC): objDirCheck obj/$(SRC).o 
	g++ obj/$(SRC).o -o obj/$(SRC)

obj/$(SRC).o: objDirCheck $(SRC).c
	g++ $(SRC).c -c -o obj/$(SRC).o

objDirCheck:
	mkdir -p obj

run: obj/$(SRC)
	./obj/$(SRC)
	#sed -i "\$$a</events>" trace.xml
	#sed -i "s/&/&amp;/g" trace.xml
	#nm -n ./obj/$(SRC) > addresses
	
clean:
	rm -rf obj
	rm -f trace.xml types.xml $(SRC).cil.c *matrix addresses *xml

checkxml:
	xmllint --schema ../../xml/trace-schema.xsd --noout trace.xml 

all: run checkxml
