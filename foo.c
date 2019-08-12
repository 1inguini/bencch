#include <stdio.h>

long foo(void) {
    printf("function foo called\n");
    return 100;
}

long bar(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7,
         long a8, long a9, long b0, long b1, long b2, long b3, long b4, long b5,
         long b6, long b7, long b8, long b9) {
    long sum = a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
    return printf(
        "bar(%lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, %lo, "
        "%lo, %lo, %lo, %lo, %lo, %lo, %lo), sum = %lo\n",
        a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, b0, b1, b2, b3, b4, b5, b6, b7,
        b8, b9, sum);
}

long callbar(long a0, long a1, long a2, long a3, long a4, long a5, long a6,
             long a7, long a8, long a9, long b0, long b1, long b2, long b3,
             long b4, long b5, long b6, long b7, long b8, long b9) {
    bar(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, b0, b1, b2, b3, b4, b5, b6, b7,
        b8, b9);
    return 0;
}
