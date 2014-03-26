JAR=joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar
JAVA_OPT=-Xss10m

.PHONY: clean zip

all: joosc

$(JAR): $(wildcard */*/*/*/*/*/*.scala)
	sbt "joosbox-compiler/assembly"

joosc: $(JAR)
	@echo "#!/bin/sh\n\
java $(JAVA_OPT) -jar $(JAR) \$$@" > joosc
	chmod +x joosc

clean:
	rm -f joosc
	sbt clean

zip: psobot-cktaylor-wlue.zip

psobot-cktaylor-wlue.zip: clean joosbox-compiler joosbox-core joosbox-lexer joosbox-parser project scripts Makefile README.md joos1w.lr1
	rm -f $@
	zip -x "joosbox-compiler/src/test/resources/marmoset-tests/*" -x "joosbox-compiler/src/test/resources/stdlib/runtime.mach.s" -x clean -x "*.class" -x "*.DS_Store" -x "project/project/*" -x "project/target/*" -r $@ $^
