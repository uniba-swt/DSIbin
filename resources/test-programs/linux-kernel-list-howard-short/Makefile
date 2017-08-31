
SRC=linux-kernel-list-howard-short
INST_DIR=../../cil-inst

obj/$(SRC): objDirCheck obj/inst_util.o obj/$(SRC).o 
	gcc obj/$(SRC).o obj/inst_util.o -o obj/$(SRC)

obj/$(SRC).o: objDirCheck $(SRC).c
	$(CILLY_BIN) -g --save-temps=obj --noPrintLn $(SRC).c --doinstrument -c -o obj/$(SRC).o
	cp obj/$(SRC).cil.c .

obj/inst_util.o: $(INST_DIR)/inst_util.c $(INST_DIR)/inst_util.h
	gcc $(INST_DIR)/inst_util.c -c -o obj/inst_util.o

objDirCheck:
	mkdir -p obj

run: obj/$(SRC)
	./obj/$(SRC) 999
	sed -i "\$$a</events>" trace.xml
	sed -i "s/&/&amp;/g" trace.xml
	nm -n ./obj/$(SRC) > addresses
	
clean:
	rm -rf obj
	rm -f trace.xml types.xml $(SRC).cil.c *matrix addresses *xml

checkxml:
	xmllint --schema ../../xml/trace-schema.xsd --noout trace.xml 

all: run checkxml
