
#! /bin/sh

dist_bin=$(dirname $(dirname $0))
code_path=""

for i in $dist_bin/lib/*; do
code_path=" -pa $i/ebin $code_path";
done

erl $code_path -config $dist_bin/config/sys.config -boot $dist_bin/releases/0.8.5/sinan  -run sin_builder main $@ -sinan autokill autokill -noshell

