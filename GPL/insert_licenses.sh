#!/bin/bash

pylicense="$( dirname "${BASH_SOURCE[0]}" )/header.py.txt"
cpplicense="$( dirname "${BASH_SOURCE[0]}" )/header.cpp.txt"
f90license="$( dirname "${BASH_SOURCE[0]}" )/header.f90.txt"
script_dir="$( dirname "${BASH_SOURCE[0]}" )/../launcher/scripts"

#prefix_files=$( find . -iname "*.py" | xargs grep -lv '^#!' )
#infix_files=$( find . -iname "*.py" | xargs grep -l '^#!' )
#
#
#for f in $prefix_files
#do
#    echo -e "0r $pylicense\nw" | ed $f
#done
#
##NEEDS FIXED
##for f in $infix_files $script_dir/*
##do
##    echo -e "1r $pylicense\nw" | ed $f
##done

#prefix_files=$( find . -iname '*.cpp' -o -iname '*.h' -o -iname '*.c' -o -iname '*.cpp' -o -iname '*.hpp' )
#echo $prefix_files
#
#for f in $prefix_files
#do
#    echo -e "0r $cpplicense\nw" | ed $f
#done

prefix_files=$( find . -iname '*.src' -o -iname '*.f90' )
echo $prefix_files

for f in $prefix_files
do
    echo -e "0r $f90license\nw" | ed $f
done

