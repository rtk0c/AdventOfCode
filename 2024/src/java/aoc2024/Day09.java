package aoc2024;

import java.util.*;

public class Day09 {
    private static int[] makeDiskMap(int[] input) {
        int diskmapLen = 0;
        for (int d : input) {
            diskmapLen += d;
        }

        var res = new int[diskmapLen];
        int idx = 0;

        boolean isFile = true;
        int fileId = 0;
        for (int d : input) {
            int elm = isFile ? fileId++ : -1;
            isFile = !isFile;
            for (int i = 0; i < d; ++i)
                res[idx++] = elm;
        }

        return res;
    }

    private static long checksum(int[] diskmap) {
        long prod = 0;
        for (int i = 0; i < diskmap.length; ++i) {
            if (diskmap[i] == -1)
                continue;
            prod += i * diskmap[i];
        }
        return prod;
    }

    public static long solvePart1(int[] input) {
        var disk = makeDiskMap(input);
        int writep = 0;
        int readp = disk.length - 1;

        for (;;) {
            while (disk[writep] != -1)
                writep++;
            while (disk[readp] == -1)
                readp--;

            if (writep >= readp)
                break;

            disk[writep++] = disk[readp];
            disk[readp--] = -1;
        }

        return checksum(disk);
    }
}
