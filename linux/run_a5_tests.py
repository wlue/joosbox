#!/usr/bin/env python
import subprocess

tests = [
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_return_1.java"],
        "", 1
    )
]


def single_test(files, expected_output, expected_return):
    p = subprocess.Popen(
        ['./run_codegen_test.sh'] + files,
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        cwd="/vagrant/linux"
    )
    stdout, stderr = p.communicate()
    retval = p.returncode
    if expected_return != retval:
        raise ValueError(
            "Test %s failed:\n\tExpected returncode: %d\n\tGot returncode: %d"
            % (files, expected_return, retval)
        )
    if expected_output != stdout:
        raise ValueError(
            "Test %s failed:\n\tExpected output: %s\n\tGot output: %s"
            % (files, expected_output, stdout)
        )


def run():
    passed = 0
    print "Starting test run (%d tests)..." % (len(tests))
    for test in tests:
        try:
            single_test(*test)
            print "Test '%s' passed." % test[0][0]
            passed += 1
        except ValueError as e:
            print e
    print "%d / %d tests passed (%f%%)" % \
        (
            passed,
            len(tests),
            (float(passed) / len(tests)) * 100.0
        )

if __name__ == "__main__":
    run()
