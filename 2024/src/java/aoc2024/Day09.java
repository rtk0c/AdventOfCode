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
                break;
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

    private static int premeasureCompactMap(int[] input) {
        int entries = 0;
        boolean isFile = true;
        for (int d : input) {
            // At voids, reserve enough space for max number of files relocated into it
            entries += isFile ? 1 : d;
            isFile = !isFile;
        }
        return entries;
    }

    private static void makeCompactMap(int[] input, int[] fileIds, int[] lengths) {
        boolean isFile = true;
        int fileId = 0;
        int w = 0; // write index
        for (int i = 0; i < input.length; ++i) {
            if (isFile) {
                fileIds[w] = fileId++;
                lengths[w++] = input[i];
            } else {
                int w0 = w;
                int voidSize = input[i];
                // Insert input[i]-number of 0-length voids
                for (int j = 0; j < voidSize; ++j) {
                    fileIds[w] = -1;
                    lengths[w++] = 0;
                }
                if (voidSize > 0)
                    lengths[w0] = voidSize;
            }
            isFile = !isFile;
        }
    }

    private static long checksum(int[] fileIds, int[] lengths) {
        long prod = 0;
        final int len = fileIds.length;
        int pos = 0;
        for (int i = 0; i < len; ++i) {
            if (fileIds[i] == -1)
                pos += lengths[i];
            else
                for (int j = 0; j < lengths[i]; ++j)
                    prod += (pos++) * fileIds[i];
        }
        return prod;
    }

    public static long solvePart2(int[] input) {
        int entries = premeasureCompactMap(input);
        int[] fileIds = new int[entries];
        int[] lengths = new int[entries];
        makeCompactMap(input, fileIds, lengths);

        for (int i = entries - 1; i >= 0; --i) {
            if (fileIds[i] == -1)
                continue;

            // Search from the left for a suitable spot
            int j = 0;
            final int fileLen = lengths[i];
            while (j < i && (fileIds[j] != -1 || lengths[j] < fileLen))
                j++;

            // Search failed, skip this file
            if (j == i)
                continue;
            // Search succeeded, copy file over
            fileIds[j] = fileIds[i];
            fileIds[i] = -1;
            int voidLen = lengths[j];
            lengths[j] = fileLen;
            if (fileIds[j + 1] == -1) {
                lengths[j + 1] = voidLen - fileLen;
            }
        }

        return checksum(fileIds, lengths);
    }
}
