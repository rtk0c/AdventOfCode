package aoc2024.day17;

import java.util.*;

public class Processor {
    public long a, b, c;

    // Program: 2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0
    public static final int ADV = 0;
    public static final int BXL = 1;
    public static final int BST = 2;
    public static final int JNZ = 3;
    public static final int BXC = 4;
    public static final int OUT = 5;
    public static final int BDV = 6;
    public static final int CDV = 7;

    public void reset() { reset(0, 0, 0); }
    public void reset(long a, long b, long c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    public long[] run(int[] prog) {
        int pc = 0;
        var out = new ArrayList<Long>();
        while (pc < prog.length) {
            int arg = prog[pc + 1];
            switch (prog[pc]) {
                case ADV -> a = div(arg);
                case BDV -> b = div(arg);
                case CDV -> c = div(arg);
                case BXL -> b = b ^ arg;
                case BXC -> b = b ^ c;
                case BST -> b = comboDecode(arg) % 8;
                case OUT -> out.add(comboDecode(arg) % 8);
                case JNZ -> {
                    if (a != 0) {
                        pc = arg;
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
