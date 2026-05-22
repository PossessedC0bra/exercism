class Darts {
    int score(double xOfDart, double yOfDart) {
        double dist = Math.sqrt((xOfDart * xOfDart) + (yOfDart * yOfDart));
        
        if (dist <= 1.0) {
            return 10;
        } else if (dist <= 5.0) {
            return 5;
        } else if (dist <= 10.0) {
            return 1;
        }

        return 0;
    }
}
