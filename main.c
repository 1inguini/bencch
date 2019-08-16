
long example() {
    long x;
    x;
    long *y;
    y;
    &x;
    y = &x;
    *y;
    *y = 3;
    return x;
}  // → 3
