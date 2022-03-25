TheCompiler=$(realpath $(dirname "$0")/../propeller.native)
TheWrapper=$(realpath $(dirname "$0")/../prc.sh)

# CheckExpectWReturnCode Exec ExpectedOutput ExpectedStderr ExpectedReturnCode
# Exec: command to execute
# ExpectedOutput: Reference output. Pass "IGNORED" to ignore all output.
# ExpectedStderr: Reference output of stderr. Pass "IGNORED" to ignore all output to stderr.
# ExpectedReturnCode: Expected return code. Pass "FAIL" to accept any non-zero return code.
CheckExpectWReturnCode() {
  RETCODE=0
  EXEC=$1
  ExpectedOutput=$2
  ExpectedStderr=$3
  ExpectedRet=$4
  OutputT=$(mktemp prtest.XXXXX)
  StderrT=$(mktemp prtest.XXXXX)
  Output=$(mktemp prtest.XXXXX)
  Stderr=$(mktemp prtest.XXXXX)

  eval "${EXEC} > ${OutputT} 2> ${StderrT}"
  Ret=$?
  tr -d '\015' < ${OutputT} > ${Output}
  tr -d '\015' < ${StderrT} > ${Stderr}

  [[ ${ExpectedOutput} == IGNORED ]] || diff -u ${ExpectedOutput} ${Output} || RETCODE=1
  [[ ${ExpectedStderr} == IGNORED ]] || diff -u ${ExpectedStderr} ${Stderr} || RETCODE=1

  if [[ ${ExpectedRet} == FAIL ]]; then
    [ $Ret -ne 0 ] || RETCODE=1
  else
    [ $Ret -eq $ExpectedRet ] || RETCODE=1
  fi
  
  rm ${OutputT} ${StderrT} ${Output} ${Stderr}
  return $RETCODE
}

# CheckExpect Exec ExpectedOutput
# convenience function
CheckExpect() {
  EXEC=$1
  ExpectedOutput=$2
  CheckExpectWReturnCode "$EXEC" $ExpectedOutput IGNORED 0
}

# CheckFail Exec ExpectedStderr
# convenience function
CheckFail() {
  EXEC=$1
  ExpectedStderr=$2
  CheckExpectWReturnCode "$EXEC" IGNORED $ExpectedStderr FAIL
}


# CompiledCheckExpectWReturnCode Source ExpectedOutput ExpectedStderrExpectedReturnCode
# convenience function. Compile the given propeller source program and run it with CheckExpectedWReturnCode
CompiledCheckExpectWReturnCode() {
  SRC=$1
  shift
  BASENAME=${SRC%.pr}
  EXEC=$BASENAME.out
  [[ $UNAME =~ (CYGWIN|MINGW|MSYS).* ]] && EXEC=$BASENAME.exe
  ${TheWrapper} $SRC
  CheckExpectWReturnCode "$EXEC" $@
  rm $EXEC
}

# CompiledCheckFail Exec ExpectedStderr
# convenience function
CompiledCheckExpect() {
  SRC=$1
  ExpectedOutput=$2
  CompiledCheckExpectWReturnCode $SRC $ExpectedOutput IGNORED 0
}

# CompiledCheckFail Exec ExpectedStderr
# convenience function
CompiledCheckFail() {
  SRC=$1
  ExpectedStderr=$2
  CompiledCheckExpectWReturnCode $SRC IGNORED $ExpectedStderr 0
}
