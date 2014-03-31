#!/bin/bash

nasm -O1 -f macho -g -F null joosbox-compiler/src/test/resources/stdlib/runtime.mach.s -o output/runtime.o
for x in output/*.s
do
  nasm -O1 -f macho -g -F null $x -o output/$(basename $x).o
done
gcc -Wl,-no_pie -m32 -o main output/*.o -Wl,-e,_start