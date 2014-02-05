JAR=joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar

.PHONY: clean prepare_upload

all: joosc

$(JAR):
	sbt "joosbox-compiler/assembly"

joosc: $(JAR)
	@echo "#!/bin/sh\n\
java -jar $(JAR) \$$@" > joosc
	chmod +x joosc

clean:
	rm -f joosc
	sbt clean

psobot-cktaylor-wlue.zip: joosbox-compiler joosbox-core joosbox-lexer joosbox-parser project Makefile README.md joos1w.lr1
	rm -f $@
	zip -x "joosbox-compiler/src/test/resources/marmoset-tests/*" -x clean -x "*.DS_Store" -x "project/project/*" -x "project/target/*" -r $@ $^