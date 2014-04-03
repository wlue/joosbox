public class compiler_null_cast {
    public compiler_null_cast() {}

    public static int test() {
        Object a = null;
        String b = (String) a;
        if (b == null) {
            return 123;
        } else {
            return 14;
        }
    }
}
