// CODE_GENERATION
public class J1_stringconcat {

    public static int k = 0;
    public String s;

    public J1_stringconcat (String str) {
        this.s = str+str+str;
    }

    public static int test() {
        int i=0;
        while (J1_stringconcat.k != 0) {
            i = i+1;
        }
        for (int j=0; j<10; j=j+1) {
        }
        return 123;
    }

}
