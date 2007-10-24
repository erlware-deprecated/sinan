#! /bin/sh

dist_bin=$(dirname $(dirname $0))
code_path=""

for i in $dist_bin/lib/*; do
code_path=" -pa $i/ebin $code_path";
done

erl $code_path -run pjg_gen gen $@ -run init stop -noshell

