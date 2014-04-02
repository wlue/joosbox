// PARSER_WEEDER,CODE_GENERATION
public class compiler_static_invocation {
    public compiler_static_invocation() {}
    public static int test() {
        int r1 = compiler_static_invocation.m0(0);
        return 113 + r1;
    }

    public static int m0(int a) {
        int r = 0;
        if (a == 0) r = 10;
        return r;
    }
}
