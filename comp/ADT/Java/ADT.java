// Example of sum types
//   Very easy - goes back to its C heritage
class Animal {
    int numberLegs;
    String name;
}

class Eight {
    boolean ones;
    boolean twos;
    boolean fours;
}

// Example of product types
//   Rather forced - In Java data types like to be
//                   either one thing or another.
class IntOrString {
    private IntOrString() {}
    class AnInt extends IntOrString {
        int i;
        AnInt(int i) { this.i = i; }
        Boolean isInt() { return true; }
        Boolean isString() { return false; }
    }
    class AnString extends IntOrString {
        String s;
        AnString(String s) { this.s = s; }
        Boolean isInt() { return false; }
        Boolean isString() { return true; }
    }

}

public class ADT {
    public static void main(String[] args) {
        System.out.println("See if things even compile");
    }
}
