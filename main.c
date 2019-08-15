
long example() {
    long x;
    long *y;
    y = &x;
    *y = 3;
    return x;
}  // → 3
