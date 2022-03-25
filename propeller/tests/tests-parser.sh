#!/usr/bin/env bash
source $(dirname "$0")/testutil.sh
cd $(dirname "$0")/parser

for i in test-*.pr
do
  out=${i%.pr}.out
  if CheckExpect "$TheCompiler -a $i" $out; then
    echo "$i passed"
  else
    echo "$i failed"
  fi
done

for i in fail-*.pr
do
  err=${i%.pr}.err
  if CheckFail "$TheCompiler -a $i" $err; then
    echo "$i passed"
  else
    echo "$i failed"
  fi
done
