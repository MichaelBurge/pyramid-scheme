#!/bin/sh

for file in tests/*.pmd
do
    ./pyramid -t $file
done
