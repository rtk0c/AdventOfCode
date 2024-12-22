package aoc2024.day19;

import java.util.*;

// Quick and dirty 2-depth trie designed for day 19
public class QndTrie {
    // 1st dim: 0-4 mapping squared to w,u,b,r,g
    // 2nd dim: list of strings beginning with those two chars
    String[][] entries;
    // Single color towels are marked here
    String[][] singles;

    private static int index(char c) {
        return switch (c) {
            case 'w' -> 0;
            case 'u' -> 1;
            case 'b' -> 2;
            case 'r' -> 3;
            case 'g' -> 4;
            default -> -1;
        };
    }

    private static int index(String s, int baseI) {
        assert s.length() >= 2;
        char c0 = s.charAt(baseI);
        char c1 = s.charAt(baseI + 1);
        return index(c1) * 5 + index(c0);
    }

    private static String[] EMPTY = new String[0];

    @SuppressWarnings("unchecked")
    private static ArrayList<String>[] builder(int len) {
        var intm = new ArrayList[len];
        for (int i = 0; i < len; ++i)
            intm[i] = new ArrayList<String>();
        return intm;
    }

    private static String[][] build(ArrayList<String>[] intm) {
        var res = new String[intm.length][];
        for (int i = 0; i < intm.length; ++i)
            res[i] = intm[i].toArray(new String[0]);
        return res;
    }

    public QndTrie(String[] needles) {
        var entries = builder(5*5);
        var singles = builder(5);

        for (var needle : needles) {
            if (needle.length() > 1) {
                entries[index(needle, 0)].add(needle);
            } else {
                int c0 = index(needle.charAt(0));
                for (int c1 = 0; c1 < 5; ++c1) {
                    entries[c1*5 + c0].add(needle);
                }
                singles[c0].add(needle);
            }
        }

        this.entries = build(entries);
        this.singles = build(singles);
    }

    public String[] prefixes(String haystack, int baseI) {
        if (haystack.length() - baseI > 1) {
            return entries[index(haystack, baseI)];
        } else {
            return singles[index(haystack.charAt(baseI))];
        }
    }
}
