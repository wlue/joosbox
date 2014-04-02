public class compiler_override {
    public compiler_override() {}

    public static int test() {
        Object x = new compiler_override();
        return x.hashCode();
    }

    public int hashCode() {
        return 43;
    }
}
