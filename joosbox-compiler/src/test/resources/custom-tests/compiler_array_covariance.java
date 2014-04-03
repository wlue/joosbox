public class compiler_array_covariance {
    public compiler_array_covariance() {}

    public static int test() {
      String[] myArray = new String[123];
      Object[] myOtherArray = myArray;
      String[] myArrayAgain = myOtherArray;
      return myArrayAgain.length;
    }
}
