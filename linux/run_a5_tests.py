#!/usr/bin/env python
import subprocess
import socket
import sys
import os

on_mac = os.uname()[0] == "Darwin"

if "vagrant" in socket.gethostname():
    cwd = "/vagrant/linux"
elif on_mac:
    cwd = (os.getcwd() + "/mac")
else:
    cwd = (os.getcwd() + "/linux")
tests = [
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_div0.java"],
        "", 13
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_neg_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_if_case_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_if_case_2.java"],
        "", 2
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_if_case_else_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_if_case_else_2.java"],
        "", 2
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_conditional_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_while_loop_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_while_loop_2.java"],
        "", 2
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_variable_access.java"],
        "", 11
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_return_1.java"],
        "", 2
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_add_1_2.java"],
        "", 3
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_expression_1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_assignment_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_method_args_1.java"],
        "", 1
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_constructor_args_1.java"],
        "", 13
    ),
    (
        ["joosbox-compiler/src/test/resources/custom-tests/compiler_class_creation_1.java"],
        "", 42
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_01.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_1_Instanceof_InLazyExp.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_1_Instanceof_OfAdditiveExpression.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_1_Instanceof_OfCastExpression.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_300locals.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_6_Assignable_Object_ObjectArray.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_6_AssignmentInArrayLength.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_AddressNotEqual.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_ArrayBaseInAssignment.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_ArrayStoreLoad.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_AssignmentInLazyOr.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_BooleanArray_External.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_CloneWithArgs.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_Complement_SideEffect.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_ConcatInSimpleInvoke.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_ConcatInStaticInvoke.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_Conditionals_NoInstructionAfterIfElse.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_FieldInitialization_Before.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_FieldInitialization_NonConstant_Before.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_GreaterOrEqual.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_LazyEagerAndOr.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_LazyEval.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_StringConstAEQ_ANE.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_String_ByteShortCharInt.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_ArrayCreateAndIndex.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_BigByteInit.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_BigCharCharInit.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_BigShortFromByteInit.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_BigShortInit.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_Hello.java"],
        "Hello, World!", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NamedTypeArray.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeByteCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeCharCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeIntCast1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeIntCast2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeOneByteByteCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeOneByteCharCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeOneByteIntCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeOneByteShortCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_NegativeShortCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_SimpleTypeArray.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_SmallInt.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_StaticField_AccessFromClass.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_StringCast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_WildConcat.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_arithmeticoperations.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_array.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_arrayAccess.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_arrayinstanceof1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_arrayinstanceof2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_backwardRef.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_charadd.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_concatInMethods.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_concat_in_binop.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_constructorbodycast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_divdiv.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_fieldinit.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_fieldinit_forward_ref.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_fieldinit_forward_ref2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_implicitstringconcatenation.java"],
        "foo117truenullz ", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_instanceof_array.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_instanceof_array2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_intstringadd.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_minuschar.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_minusminusminus.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_negativeintcast3.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_nestedcast.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_random_arithmetic.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_random_arithmetic_var.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect3.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect4.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect5.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect6.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect7.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffect8.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_array.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_array2.java"],
        "r=123", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_array3.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_array4.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj3.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sim_and.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sim_or.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sim_xor.java"],
        "All your base are belong to us!", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_staticMethodInvocation.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_stringadd.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_stringconcat.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_toomuchinc.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_typecheck_array.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_typecheck_expstm.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_typecheck_plus.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_while1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_while2.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_whiletrue1.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_A_CastToArray.java"],
        "", 13
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_A_CastToString.java"],
        "", 13
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_divisionbyzero.java"],
        "", 13
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J2_A_FieldInitialization_Static_Before.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J2_A_FieldInitialization_Static_NonConstant_Before.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J2_fieldinit_forward_ref.java"],
        "", 123
    ),
    (
        ["joosbox-compiler/src/test/resources/marmoset-tests/a5/J2_forwardRef.java"],
        "", 123
    ),
    (
        [
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_A_CastNewExp/Main.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_A_CastNewExp/javax/swing/JButton.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1e_A_CastNewExp/javax/swing/JComponent.java"
        ],
        "", 13
    ),
    (
        [
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj/Main.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj/java/util/Collection.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj/java/util/LinkedList.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_sideeffects_obj/java/util/List.java"
        ],
        "", 123
    ),
    (
        [
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_callbeforereturn/Main.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_callbeforereturn/java/util/Collection.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_callbeforereturn/java/util/LinkedList.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_callbeforereturn/java/util/List.java"
        ],
        "", 123
    ),
    (
        [
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_CloneOnInterface/Main.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_CloneOnInterface/pkg/Class.java",
            "joosbox-compiler/src/test/resources/marmoset-tests/a5/J1_A_CloneOnInterface/pkg/Interface.java",
        ],
        "", 123
    ),
]


def single_test(files, expected_output, expected_return):
    p = subprocess.Popen(
        ['./run_codegen_test%s.sh' % ("_local" if on_mac else "")] + files,
        stdout=subprocess.PIPE, cwd=cwd
    )
    stdout, stderr = p.communicate()
    retval = p.returncode

    if retval != 0:
        raise ValueError("Compilation return error %d:\n%s" % (retval, stdout))

    p = subprocess.Popen(['../main'], stdout=subprocess.PIPE, cwd=cwd)
    stdout, stderr = p.communicate()
    retval = p.returncode

    print "Test returned: %d" % retval
    if expected_return != retval:
        raise ValueError(
            "Test %s failed:\n\tExpected returncode: %d\n\tGot returncode: %d"
            % (files[0], expected_return, retval)
        )
    if expected_output != stdout:
        raise ValueError(
            "Test %s failed:\n\tExpected output: %s\n\tGot output: %s"
            % (files[0], expected_output, stdout)
        )


def run(cont=False):
    print "Running tests in directory: %s" % cwd
    passed = 0
    run = 0
    print "Starting test run (%d tests)..." % (len(tests))
    for test in tests:
        try:
            print "Running test: %s" % test[0][0]
            single_test(*test)
            print "Test '%s' passed." % test[0][0]
            passed += 1
        except ValueError as e:
            print "Test '%s' failed." % test[0][0]
            print e
            if not cont:
                break
        except KeyboardInterrupt:
            break
        finally:
            run += 1
    print "%d / %d tests passed (%f%%)" % \
        (
            passed, run,
            (float(passed) / run) * 100.0
        )

if __name__ == "__main__":
    run(len(sys.argv) > 1 and sys.argv[1] == "continue")
