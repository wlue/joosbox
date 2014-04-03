public class compiler_instance_field_access {
    

    public compiler_instance_field_access() {}

    public int z = 10;
    public compiler_instance_field_access contained = null;

    public static int test() {
        compiler_instance_field_access a = new compiler_instance_field_access();

        a.contained = new compiler_instance_field_access();
        a.contained.z = 10;

        int x = 4;
        int y = 7;
        return x + y + a.contained.z;
    }
}
