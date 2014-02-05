JAR=joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar

.PHONY: clean

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
