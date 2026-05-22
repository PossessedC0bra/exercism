import java.util.List;
import java.util.ArrayList;

class DnDCharacter {

    private int strength;
    private int dexterity;
    private int constitution;
    private int intelligence;
    private int wisdom;
    private int charisma;
    private int hitpoints;

    public DnDCharacter() {
        strength = ability(rollDice());
        dexterity = ability(rollDice());
        constitution = ability(rollDice());
        intelligence = ability(rollDice());
        wisdom = ability(rollDice());
        charisma = ability(rollDice());
        hitpoints = 10 + modifier(constitution);
    }
    
    int ability(List<Integer> scores) {
        int minIdx = 0;
        for (int i = 1; i < scores.size(); i++) {
            if (scores.get(minIdx) > scores.get(i)) {
                minIdx = i;
            }
        }

        int sum = 0;
        for (int i = 0; i < scores.size(); i++) {
            if (i == minIdx) {
                continue;
            }

            sum += scores.get(i);
        }

        return sum;
    }

    List<Integer> rollDice() {
        ArrayList<Integer> ints = new ArrayList<>(4);
        for (int i = 0; i < 4; i++) {
            ints.add(randomInt(1, 6));
        }
        return ints;
    }

    private int randomInt(int min, int max) {
        return (int) ((Math.random() * (max - min)) + min);
    }

    int modifier(int input) {
        return (int) Math.floor(((input - 10.0) / 2.0));
    }

    int getStrength() {
        return strength;
    }

    int getDexterity() {
        return dexterity;
    }

    int getConstitution() {
        return constitution;
    }

    int getIntelligence() {
        return intelligence;
    }

    int getWisdom() {
        return wisdom;
    }

    int getCharisma() {
        return charisma;
    }

    int getHitpoints() {
        return hitpoints;
    }
}
