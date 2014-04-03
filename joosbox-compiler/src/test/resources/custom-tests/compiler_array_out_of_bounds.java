public class compiler_array_out_of_bounds {
    public compiler_array_out_of_bounds() {}
    public static int test() {
      int[] x = new int[5];
      x[0] = 0;
      x[6] = 1;
      return x[0];
    }
}
