package A.B.C.D;

public class compiler_static_field_assign {
    public static int SOMETHING = 0;
    public compiler_static_field_assign() {}
    public static int test() {
        A.B.C.D.compiler_static_field_assign.SOMETHING = 69;
        return compiler_static_field_assign.SOMETHING;
    }
}
