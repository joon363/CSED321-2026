#!/bin/bash

echo "컴파일을 시작합니다..."

# 1. mli 인터페이스 파일 컴파일
ocamlc -c hw1.mli

# 2. ml 구현 파일 컴파일
ocamlc -c hw1.ml

# 3. 테스트 파일 컴파일
ocamlc -c test.ml

# 4. 실행 파일로 링킹
ocamlc -o test_runner hw1.cmo test.cmo

if [ $? -eq 0 ]; then
  echo "컴파일 성공! 테스트를 실행합니다."
  ./test_runner
else
  echo "컴파일에 실패했습니다. 코드를 확인해주세요."
fi