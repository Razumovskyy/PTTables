#!/bin/bash

fin_ext=$1

for file in ./PT_CALC/*.PT; do 
    mv -- "$file" "${file%.PT}.$fin_ext"
done