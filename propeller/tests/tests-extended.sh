#!/usr/bin/env bash
source $(dirname "$0")/testutil.sh
cd $(dirname "$0")/exttest

failed=0

for i in test-*.pr
do
  out=${i%.pr}.exp
  if CompiledCheckExpect "$i" $out; then
    echo "$i passed"
  else
    echo "$i failed"
    failed=$(($failed+1))
  fi
done

for i in fail-*.pr
do
  err=${i%.pr}.err
  if CheckFail "$TheCompiler $i" $err; then
    echo "$i passed"
  else
    echo "$i failed"
    failed=$(($failed+1))
  fi
done

if [ $failed == 0 ]
then
  echo "All tests passed."
else
  echo "$failed test(s) failed."
fi
