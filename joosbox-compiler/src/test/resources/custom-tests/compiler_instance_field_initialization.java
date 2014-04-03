public class compiler_instance_field_initialization {
    

    public compiler_instance_field_initialization() {}

    public int ten = 10;
    public int eleven = 11;
    public int twelve = 12;
    public compiler_instance_field_initialization contained = null;

    public static int test() {
        compiler_instance_field_initialization a = new compiler_instance_field_initialization();
        a.contained = new compiler_instance_field_initialization();

        return a.ten + a.eleven + a.twelve + a.contained.ten + a.contained.eleven + a.contained.twelve;
    }
}
