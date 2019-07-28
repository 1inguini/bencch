// mからnまでを足す
int sum(m, n) {
    int acc = 0;
    for (int i = m; i <= n; i = i + 1) acc = acc + i;
    return acc;
}

int main() {
    return sum(1, 10);  // 55を返す
}
