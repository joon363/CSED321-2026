#!/bin/bash

# hw4 디렉토리로 이동 (hw4 디렉토리가 있다면)
if [ -d "hw4" ]; then
    cp test.ml hw4/
    cd hw4
fi

echo "Building library (lib.cma)..."
make clean > /dev/null 2>&1
make > /dev/null 2>&1

echo "Building and running test..."
# ocamlc를 사용하여 테스트 코드를 빌드. 기존 Makefile의 -thread 옵션을 사용.
ocamlc -thread -I . -o test_runner lib.cma test.ml

if [ $? -eq 0 ]; then
    ./test_runner
else
    echo "Test compilation failed. Please check if your code compiles with 'make'."
fi