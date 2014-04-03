public class compiler_null_field {
    public compiler_null_field() {}
    public int value = 10;
    public static int test() {
        compiler_null_field obj = null;
        return obj.value;
    }
}
