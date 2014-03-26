#!/bin/bash

set -e
cd ..

mkdir -p output
rm -f main
rm -f output/*.s output/*.o
make
./joosc $@ joosbox-compiler/src/test/resources/stdlib/*/*/*.java
nasm -O1 -f macho -g -F null joosbox-compiler/src/test/resources/stdlib/runtime.mach.s -o output/runtime.o
for x in output/*.s
do
  nasm -O1 -f macho -g -F null $x -o output/$(basename $x).o
done
gcc -e _start -Wl,-no_pie -m32 -o main output/*.o
./main

