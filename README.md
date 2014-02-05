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

