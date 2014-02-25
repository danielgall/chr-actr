#!/bin/bash
#CHR_ACTR_PATH=`dirname $BASH_SOURCE`
CHR_ACTR_PATH="$(dirname "$(readlink -f "$0")")"
echo $CHR_ACTR_PATH
swipl -p chractr=.:$CHR_ACTR_PATH -s $1
