public class compiler_static_field_init {
    public static int SOMETHING = 66;
    public compiler_static_field_init() {}
    public static int test() {
        return compiler_static_field_init.SOMETHING;
    }
}
