public class compiler_interface_override {
    public compiler_interface_override() {}

    public static int test() {
        java.io.Serializable x = new compiler_interface_override();
        return x.hashCode();
    }

    public int hashCode() {
        return 43;
    }
}
