#!/bin/sh

find . -name '*.rkt' -o -name '*.pmd' | xargs wc -l
