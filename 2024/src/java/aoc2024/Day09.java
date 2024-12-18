package aoc2024;

import java.util.*;

public class Day09 {
    private static Inode[] preprocess(int[] data) {
        var input = new Inode[data.length];
        int i = 0;
        int fileId = 0;
        boolean isFile = true;
        for (int d : data) {
            if (isFile)
                input[i++] = new Inode(fileId++, d);
            else
                input[i++] = new Inode(-1, d);
            isFile = !isFile;
        }
        return input;
    }

    public static long solvePart1(int[] data) {
        var input = preprocess(data);
        var out = new ArrayList<Inode>();

        int copyPtr = 0;
        int fillPtr = input.length - 1;

        while (copyPtr < fillPtr) {
            out.add(input[copyPtr]);
            int voidLen = input[copyPtr + 1].length;

            while (voidLen != 0) {
                var fill = input[fillPtr];
                if (voidLen >= fill.length) {
                    out.add(fill);
                    voidLen -= fill.length;
                    // Inode object used by output
                    /*fill.length = 0;*/
                    input[fillPtr] = null;
                    fillPtr -= 2;
                } else {
                    out.add(new Inode(fill.id, voidLen));
                    fill.length -= voidLen;
                    voidLen = 0;
                }
            }

            copyPtr += 2;
        }
        if (copyPtr == fillPtr) {
            out.add(input[fillPtr]);
        }

        //System.out.println(Arrays.toString(input));
        //System.out.println(out);

        long prod = 0;
        int pos = 0;
        for (Inode f : out) {
            for (int i = 0; i < f.length; ++i) {
                prod += (pos + i) * f.id;
            }
            pos += f.length;
        }

        return prod;
    }
}

// Call it this because why not
class Inode {
    int id;
    int length;

    public Inode(int id, int length) {
        this.id = id;
        this.length = length;
    }

    // Mark empty space
    public Inode(int length) {
        this.id = -1;
        this.length = length;
    }

    @Override
    public String toString() {
        return "{" + id + "}" + length;
    }
}
