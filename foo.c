#include <stdio.h>

int foo(void) {
    printf("function foo called\n");
    return 100;
}

int bar(int a1, int a2, int a3, int a4, int a5, int a6, int a7, int a8,
        int a9) {
    printf("bar(%d, %d, %d, %d, %d, %d, %d, %d, %d)\n", a1, a2, a3, a4, a5, a6,
           a7, a8, a9);
    return 100;
}
