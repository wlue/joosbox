#!/bin/bash

rsync --exclude=".git" --exclude="joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar" --exclude="*.class" --exclude="*/target/*" -a ./* joosbox:~/`whoami`
ssh joosbox chown -R root `whoami`
ssh joosbox "cd `whoami` && make"
ssh joosbox "cd `whoami` && ./linux/run_a5_tests.py"