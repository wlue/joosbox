Joosbox
=======
                              ______
                             / _____|
                            / /
                            | |
                            | |
                            | |
    +-------------------------------+
    |                               |
    |      __   __    __   ____     |
    |    _(  ) /  \  /  \ / ___)    |
    |   / \) \(  O )(  O )\___ \    |
    |   \____/ \__/  \__/ (____/    |
    |                               |
    |                               |
    |            JOOSBOX            |
    |                               |
    |    Joos-1W to i386 compiler   |
    |          100% SCALA           |
    |                               |
    |                               |
    |                               |
    |                               |
    |     NOT FROM CONCENTRATE      |
    |    PRODUCT OF WATERLOO, ON    |
    |                               |
    +-------------------------------+

Compiling
=========

Compile everything:

    sbt compile

Compiler the compiler:

    sbt joosbox-compiler/compile

Testing
=======

Test everything:

    sbt test

Test a particular subproject:

    sbt joosbox-lexer/test

Test a particular spec in `joosbox-lexer`.

    sbt "joosbox-lexer/test-only joosbox.lexer.test.NFASpec"

Test code generation on your local Mac:

    python ./mac/run_a5_tests.py

...or if you want the codegen tests to run fully without stopping on failure:

    python ./mac/run_a5_tests.py continue

Test code generation on a single test your local Mac:

    python ./mac/run_codegen_test_local.sh <joos file> <joos file> ... 


Running
=======

Run the compiler with command line arguments:

    sbt "joosbox-compiler/run MyProgram.joos"

Packaging
=========

Run `make`

    make

Invoke using `./joosc`

    ./joosc MyProgram.joos

