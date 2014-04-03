public class compiler_field_access {
    public int value = 42;
    public compiler_field_access() {}
    public static int test() {
        return (new compiler_field_access()).value;
    }
}
