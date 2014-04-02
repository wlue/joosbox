public class compiler_constructor_args_1 {
    public compiler_constructor_args_1() {
    }
    public compiler_constructor_args_1(int n) {
      int x = 1/n;
    }
    public static int test() {
        Object o = new compiler_constructor_args_1(0);
        return 1;
    }
}
