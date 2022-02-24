ocamlbuild -clean
ocamlbuild toplevel.native

echo "Neg Test 1" 
./toplevel.native tests/neg_test1.pr
echo "Neg Test 2" 
./toplevel.native tests/neg_test2.pr
echo "Neg Test 3" 
./toplevel.native tests/neg_test3.pr
echo "Neg Test 4" 
./toplevel.native tests/neg_test4.pr
echo "Neg Test 5" 
./toplevel.native tests/neg_test5.pr
echo "Pos Test 1" 
./toplevel.native tests/pos_test1.pr
echo "Pos Test 2" 
./toplevel.native tests/pos_test2.pr
echo "Pos Test 3" 
./toplevel.native tests/pos_test3.pr
echo "Pos Test 4" 
./toplevel.native tests/pos_test4.pr
echo "Pos Test 5" 
./toplevel.native tests/pos_test5.pr
