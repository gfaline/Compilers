ocamlbuild -clean
ocamlbuild toplevel.native


Check() {
    basename=`echo $1 | sed 's/.*\\///
                             s/.pr//'`
    correctfile="tests/${basename}.out"
    reffile="tests/${basename}.test"
    runfile="tests/${basename}.pr"

    echo "###### Testing  $runfile" 

    ./toplevel.native $runfile > $reffile
    #Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if cmp  -s $reffile $correctfile ; then
	echo "SUCCESS" 
    else
	echo "FAIL" 
    fi
    # rm $reffile
}

CheckFail() {
    basename=`echo $1 | sed 's/.*\\///
                             s/.pr//'`
    correctfile="tests/${basename}.err"
    reffile="tests/${basename}.test"
    runfile="tests/${basename}.pr"

    echo "###### Testing  $runfile" 

    ./toplevel.native $runfile 2> $reffile
    #Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if cmp  -s $reffile $correctfile ; then
    echo "SUCCESS" 
    else
    echo "FAIL" 
    fi
}

files="tests/test-*.pr tests/fail-*.pr"

for file in $files
do
    case $file in
	*test-*)
	    Check $file 
	    ;;
	*fail-*)
	    CheckFail $file 
	    ;;
	*)
    esac
done

