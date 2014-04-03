public class compiler_array_null {
    public compiler_array_null() {}
    public static int test() {
      int[] x = null;
      x[0] = 0;
      return x[0];
    }
}
