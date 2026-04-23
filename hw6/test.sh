#!/bin/bash

# hw6 디렉토리로 이동 (hw6 디렉토리가 존재한다면)
if [ -d "hw6" ]; then
    cp test1.ml test2.ml test3.ml hw6/
    cd hw6
fi

echo "Building library (lib.cma)..."
make clean > /dev/null 2>&1
make > /dev/null 2>&1

if [ $? -ne 0 ]; then
    echo "Make failed. Please check your source code compilation."
    exit 1
fi

echo "빌드가 완료되었습니다. 테스트를 실행합니다."

# ---------------------------------------------------------
# 실행을 원하는 테스트의 주석을 해제(# 제거)하고 스크립트를 실행하세요.
# ---------------------------------------------------------

# --- 문제 1 (De Bruijn Indexes) 테스트 ---
ocamlc -thread -I . -o test_runner lib.cma test1.ml && ./test_runner

# --- 문제 2 (Call-By-Value) 테스트 ---
# ocamlc -thread -I . -o test_runner lib.cma test2.ml && ./test_runner

# --- 문제 3 (Abstract Machine N) 테스트 ---
# ocamlc -thread -I . -o test_runner lib.cma test3.ml && ./test_runner