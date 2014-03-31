#!/bin/bash

make
vagrant ssh -c "cd /vagrant/linux && ./run_codegen_test.sh $@"
vagrant ssh -c "cd /vagrant && gdb ./main"
