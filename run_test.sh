#!/bin/bash

mkdir -p output
rm main
rm output/*.o
./joosc $@ joosbox-compiler/src/test/resources/stdlib/*/*/*.java
nasm -O1 -f elf -g -F dwarf joosbox-compiler/src/test/resources/stdlib/runtime.s -o output/runtime.o
for x in output/*.s
do
  nasm -O1 -f elf -g -F dwarf $x -o output/$(basename $x).o
done
ld -melf_i386 -o main output/*.o
./main
