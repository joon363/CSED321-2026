#!/bin/bash

echo "컴파일을 시작합니다..."

make clean

make

if [ $? -eq 0 ]; then
  echo "컴파일 성공! 테스트를 실행합니다."
  ./hw3
else
  echo "컴파일에 실패했습니다. 코드를 확인해주세요."
fi