public class Twofer {
    public String twofer(String name) {
        String otherName = name != null 
            ? name 
            : "you";
        
        return "One for " + otherName + ", one for me.";
    }
}
