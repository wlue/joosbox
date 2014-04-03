public class compiler_string_literal_length {
    public compiler_string_literal_length() {}
    public static int test() {
      if ("abc".length() == 3) {
        return 123;
      } else {
        return 14;
      }
    }
}
