public class compiler_null_method {
    public compiler_null_method() {}
    public static int test() {
        Boolean obj = null;
        obj.toString();
        return 0;
    }
}
