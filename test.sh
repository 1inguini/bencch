#!/bin/sh

# set -Ceux

try() {
    expected="$1"
    input="$2"

    stack run -- "$input" > tmp.asm
    nasm -o tmp.o -felf64 tmp.asm
    ld -o tmp tmp.o
    ./tmp
    # stack run "$input" | lli
    
    actual=$?
    
    if [ "$actual" = "$expected" ]; then
	echo "$input => $actual"
    else
	echo "$input => $expected expected, but got $actual"
	exit 1
    fi
}

try "$((4 + 13 - 2))" '4 + 13 - 2;';

try "$((255 - 34 + 1 - 50 + 67))" '255 - 34 + 1 - 50 + 67;'

try "$((100-3))" "100-3;"

try "$((100-(3+5)))" "100-(3+5);"

try "$((100-3*+5))" "100-3*+5;"
 
try 12 "a=12;a;"

try "$(echo $1 | bc)" "$1"

echo OK
