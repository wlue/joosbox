public class compiler_null_instanceof {
    public compiler_null_instanceof() {}

    public static int test() {
        Object a = null;
        if (a instanceof Object) {
            return 14;
        } else {
            return 123;
        }
    }
}
