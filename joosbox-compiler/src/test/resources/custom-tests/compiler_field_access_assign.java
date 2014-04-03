public class compiler_field_access_assign {
    public int value = 69;
    public compiler_field_access_assign() {}
    public static int test() {
        compiler_field_access_assign obj = new compiler_field_access_assign();
        (obj).value = 42;
        return obj.value;
    }
}
