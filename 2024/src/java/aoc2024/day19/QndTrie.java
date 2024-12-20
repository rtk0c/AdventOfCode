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

    private static int index(String s) {
        assert s.length() >= 2;
        char c0 = s.charAt(0);
        char c1 = s.charAt(1);
        return index(c1) * 5 + index(c0);
    }

    private static String[] EMPTY = new String[0];

    @SuppressWarnings("unchecked")
    public QndTrie(String[] needles) {
        ArrayList<String>[] entries = new ArrayList[5*5];
        for (int i = 0; i < entries.length; ++i)
            entries[i] = new ArrayList<String>();

        this.singles = new String[5][];
        Arrays.fill(singles, EMPTY);

        for (var needle : needles) {
            if (needle.length() > 1) {
                entries[index(needle)].add(needle);
            } else {
                int c0 = index(needle.charAt(0));
                for (int c1 = 0; c1 < 5; ++c1) {
                    entries[c1*5 + c0].add(needle);
                }
                singles[c0] = new String[] { needle };
            }
        }

        this.entries = new String[entries.length][];
        for (int i = 0; i < entries.length; ++i) {
            var al = entries[i];
            this.entries[i] = al.toArray(new String[al.size()]);
        }
    }

    public String[] prefixes(String haystack) {
        if (haystack.length() > 1) {
            return entries[index(haystack)];
        } else {
            return singles[index(haystack.charAt(0))];
        }
    }

    // Assumes haystack only contains valid chars
    public String[] startsWith(String haystack) {
        if (haystack.length() == 1) {
            return new String[] { haystack };
        }

        var res = new ArrayList<String>();
        var needles = entries[index(haystack)];
        for (var needle : needles) {
            if (haystack.startsWith(needle))
                res.add(needle);
        }
        return res.toArray(new String[0]);
    }
}
