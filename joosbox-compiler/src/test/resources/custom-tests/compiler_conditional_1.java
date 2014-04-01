public class compiler_conditional_1 {
    public compiler_conditional_1() {}
    public static int test() {
      int x = 0;
      if (x == 0 || x == 1) {
        x = -1;
      }
      if (!(x > 0 && x < 100)) {
        x = x * -1;
      }
      return x;
    }
}
