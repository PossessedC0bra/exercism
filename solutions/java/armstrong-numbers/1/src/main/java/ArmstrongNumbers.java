class ArmstrongNumbers {

    boolean isArmstrongNumber(int numberToCheck) {
        int[] digits = new int[100];

        int numberOfDigits = 0;
        int remainder = numberToCheck;
        while (remainder > 0) {
            digits[numberOfDigits++] = remainder % 10;
            remainder = remainder / 10;
        }

        
        int sum = 0;
        for (int i = 0; i < numberOfDigits; i++) {
            sum += Math.pow(digits[i], numberOfDigits);
        }

        return numberToCheck == sum;
    }
}
