package aoc2024.day17;

import java.util.*;

public class Processor {
    public long a, b, c;

    public static final int ADV = 0;
    public static final int BXL = 1;
    public static final int BST = 2;
    public static final int JNZ = 3;
    public static final int BXC = 4;
    public static final int OUT = 5;
    public static final int BDV = 6;
    public static final int CDV = 7;

    public long[] run(int[] prog) {
        int pc = 0;
        var out = new ArrayList<Long>();
        int cycles = 0;
        while (pc < prog.length) {
            cycles++;
            switch (prog[pc]) {
                case ADV -> a = div(prog[pc+1]);
                case BDV -> b = div(prog[pc+1]);
                case CDV -> c = div(prog[pc+1]);
                case BXL -> b = b ^ prog[pc+1];
                case BXC -> b = b ^ c;
                case BST -> b = comboDecode(prog[pc+1]) % 8;
                case OUT -> out.add(comboDecode(prog[pc+1]) % 8);
                case JNZ -> {
                    if (a != 0) {
                        pc = prog[pc+1];
                        continue;
                    }
                }
            }
            pc += 2;
        }

        var res = new long[out.size()];
        int i = 0;
        for (long v : out)
            res[i++] = v;
        return res;
    }

    private long comboDecode(int combo) {
        switch (combo) {
            case 4: return a;
            case 5: return b;
            case 6: return c;
            default: return combo; // 7 and above is reserved, just pretend they are literals
        }
    }

    private long div(int op) {
        long num = a;
        long denom = 1 << comboDecode(op);
        return num / denom;
    }
}
