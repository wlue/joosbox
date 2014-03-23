#!/bin/bash

vagrant ssh -c "bash -c 'cd /vagrant/linux && ./run_codegen_test.sh $@'" 2>/dev/null
