class ReverseString {

    String reverse(String inputString) {
        char[] chars = inputString.toCharArray();
        char[] reversedChars = new char[chars.length];
        for (int i = 0; i < chars.length; i++) {
            reversedChars[chars.length - 1 - i] = chars[i];
        }

        return new String(reversedChars);
    }
  
}
