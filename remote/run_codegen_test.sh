#!/bin/bash

rsync --exclude=".git" --exclude="joosbox-compiler/target/scala-2.10/joosbox-compiler-assembly-0.1-SNAPSHOT.jar" --exclude="*.class" --exclude="*/target/*" -a ./* joosbox:~/`whoami`
ssh joosbox chown -R root `whoami`
ssh joosbox "cd `whoami` && make"
ssh joosbox "bash -c 'cd /root/`whoami`/linux && ./run_codegen_test.sh $@'"

