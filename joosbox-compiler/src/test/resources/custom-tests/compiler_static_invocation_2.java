// PARSER_WEEDER,CODE_GENERATION
public class compiler_static_invocation_2 {
    public compiler_static_invocation_2() {}
    public static int test() {
        int r1 = compiler_static_invocation_2.m0(0);
        return r1;
    }

    public static int m0(int a) {
        int r = 0;
        if (a==0) r=r+1;
        if (a!=0) r=r+2;
        if (!(a==0)) r=r+4;
        if (!(a!=0)) r=r+8;
        return r;
    }
}
