#!/usr/bin/env bash
source $(dirname "$0")/testutil.sh
cd $(dirname "$0")/hello

RET=0
CompiledCheckExpect hello.pr hello.exp || { echo "hello.pr failed"; RET=1; }
CompiledCheckExpectWReturnCode hello_ret.pr hello.exp IGNORED 5 || { echo "hello_ret.pr failed"; RET=1; }
CheckFail "$TheWrapper hello_bad.pr" IGNORED || { echo "hello_bad.pr failed"; RET=1; }
[ $RET -eq 0 ] && echo "all 3 tests passed"
exit $RET
