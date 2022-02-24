ocamlbuild -clean
ocamlbuild toplevel.native

globallog=testall.log
rm -f $globallog
error=0
globalerror=0


# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.mc//'`
    reffile=`echo $1 | sed 's/.mc$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2


    Run "./toplevel.native ${basename}.pr" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

files="tests/test-*.pr tests/fail-*.pr"

for file in $files
do
    case $file in
	*test-*)
	    ./toplevel.native $file >> "${file}.out"
	    ;;
	*fail-*)
	    echo $file >> "${file}.out"
	    ;;
	*)
    esac
done

