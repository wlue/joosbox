public class compiler_instance_field_access {
    

    public compiler_instance_field_access() {}
    public int z = 10;

    public static int test() {
        compiler_instance_field_access a = new compiler_instance_field_access();
        
        int x = 4;
        int y = 7;
        return x + y + a.z;
    }
}
