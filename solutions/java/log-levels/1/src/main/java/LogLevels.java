public class LogLevels {

    public static String message(String logLine) {
        int colonIdx = logLine.indexOf(':');
        String message = logLine.substring(colonIdx + 1);
        return message.trim();
    }

    public static String logLevel(String logLine) {
        int colonIdx = logLine.indexOf(':');
        String level = logLine.substring(0, colonIdx);
        return level.substring(1, level.length() - 1).toLowerCase();
    }

    public static String reformat(String logLine) {
        String message = message(logLine);
        String level = logLevel(logLine);

        return message + " (" + level + ")";
    }
}
