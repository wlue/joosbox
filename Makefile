JAR=joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar

.PHONY: clean zip

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

zip: psobot-cktaylor-wlue.zip

psobot-cktaylor-wlue.zip: clean joosbox-compiler joosbox-core joosbox-lexer joosbox-parser project scripts Makefile README.md joos1w.lr1
	rm -f $@
	zip -x "joosbox-compiler/src/test/resources/marmoset-tests/*" -x clean -x "*.class" -x "*.DS_Store" -x "project/project/*" -x "project/target/*" -r $@ $^
