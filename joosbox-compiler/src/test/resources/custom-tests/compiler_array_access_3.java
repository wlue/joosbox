public class compiler_array_access_3 {
    public compiler_array_access_3() {}
    public static int test() {
      int[] x = new int[5];
      x[0] = 1;
      x[1] = 2;
      return x[0] + x[1];
    }
}
