public class compiler_concat_1_null_a_true_length {
    public compiler_concat_1_null_a_true_length() {}
    public static int test() {
        String x = new String("");
        x = x + 1;
        x = x + null;
        x = x + 'a';
        x = x + true;
        return x.length();
    }
}
