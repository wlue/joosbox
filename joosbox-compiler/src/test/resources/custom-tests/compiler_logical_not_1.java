public class compiler_logical_not_1 {
    public compiler_logical_not_1() {}
    public static int test() {
      int x = 0;
      if(!(x == 0)) {
        x = x + 5;
      }
      if(!(x == 1)) {
        x = x + 1;
      }
      return x;
    }
}
