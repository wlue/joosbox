public class compiler_array_covariance_error {
    public compiler_array_covariance_error() {}

    public static int test() {
      Object[] myArray = new Object[123];
      String[] myOtherArray = myArray;
      return ((String[]) myOtherArray).length;
    }
}
