#!/bin/bash

set -e

#g++ -O2 -o A A.cpp
clang++ -std=c++17 -O2 -o A A.cpp
for i in $(seq 0 19)
do
  f=$(printf %04d $i).txt
  echo -n $f
  ./A < tools/in/$f > out/$f
done
