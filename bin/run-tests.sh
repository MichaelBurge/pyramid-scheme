#!/bin/sh

tabs 30

for file in tests/*.pmd ceagle/tests/*.c
do
    echo -n "$file:\t"
    ./pyramid -t $file
done
