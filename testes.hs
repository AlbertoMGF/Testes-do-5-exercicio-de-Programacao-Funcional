import Prelude hiding (minimum,maximum)
import ProgFuncLista
import Test.HUnit

testsize1 = TestCase (assertEqual "size of empty tree" 0 (sizeBST NIL))
testsize2 = TestCase (assertEqual "size of very unbalanced tree" 5 (sizeBST (Node 3 NIL (Node 4 NIL (Node 5 NIL (Node 9 (Node 7 NIL NIL) NIL))) )))
testsize3 = TestCase (assertEqual "size of balanced tree" 3 (sizeBST (Node 3 (Node (-1) NIL NIL) (Node 4 NIL NIL))))

testisBST1 = TestCase (assertEqual "correct BST tree" True (isBST (Node 10 (Node 0 NIL (Node 5 NIL NIL)) (Node 20 (Node 15 NIL NIL) (Node 40 NIL NIL)))))
testisBST2 = TestCase (assertEqual "node with 7 in wrong position" False (isBST (Node 10 (Node 5 (Node 1 NIL NIL) (Node 6 NIL NIL)) (Node 20 (Node 7 NIL NIL) (Node 99 NIL NIL)))))
testisBST3 = TestCase (assertEqual "node with 17 in wrong position" False (isBST (Node 10 (Node 5 NIL (Node 17 NIL NIL)) (Node 20 NIL NIL))))

testinsert1 = TestCase (assertEqual "insert in left position" (Node 10 (Node 5 NIL NIL) (Node 20 (Node 17 NIL NIL) NIL)) (insert 17 (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testinsert2 = TestCase (assertEqual "insert in right position" (Node 10 (Node 5 NIL NIL) (Node 20 NIL (Node 22 NIL NIL))) (insert 22 (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testinsert3 = TestCase (assertEqual "insert in an empty tree" (Node 40 NIL NIL) (insert 40  NIL))
testinsert4 = TestCase (assertEqual "insert an element that has already in the tree" (Node 20 NIL (Node 20 NIL (Node 20 NIL NIL))) (insert 20  (Node 20 NIL (Node 20 NIL NIL))))

testsearch1 = TestCase (assertEqual "search tree's node " (Node 90 NIL (Node 100 NIL NIL)) (search 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsearch2 = TestCase (assertEqual "search tree's root" (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL))) (search 10 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsearch3 = TestCase (assertEqual "search for node that hasn't in tree" NIL (search 110 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsearch4 = TestCase (assertEqual "search for node with data in repetition in tree" (Node 110 NIL (Node 110 NIL NIL)) (search 110 (Node 10 (Node 2 NIL NIL) (Node 110 NIL (Node 110 NIL NIL)))))

testmaximum1 = TestCase (assertEqual "testmaximum1" (Node 20 NIL NIL) (maximum (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testmaximum2 = TestCase (assertEqual "testmaximum2" (Node 9 (Node (8.5) NIL NIL) NIL)  (maximum (Node 5 NIL (Node 6 NIL (Node 7 NIL (Node 8 NIL (Node 9 (Node (8.5) NIL NIL) NIL)))) ) ))

testminimum1 = TestCase (assertEqual "testminimum1" (Node 5 NIL NIL)  (minimum (Node 10 (Node 5 NIL NIL) (Node 20 NIL NIL)) ))
testminimum2 = TestCase (assertEqual "testminimum2" (Node 1 NIL (Node 1.5 NIL NIL))  (minimum (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL (Node 1.5 NIL NIL)) NIL) NIL) NIL) NIL) ))

testpredecessor1 = TestCase (assertEqual "predecessor in some lower position" 1  (predecessor 2 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL NIL) NIL) NIL) NIL) NIL) ))
testpredecessor2 = TestCase (assertEqual "predecessor in some higher position" 90  (predecessor 100 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))

testsuccessor1 = TestCase (assertEqual "successor in some lower position" 100  (successor 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testsuccessor2 = TestCase (assertEqual "successor in some higher position" 3  (successor 2 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 NIL NIL) NIL) NIL) NIL) NIL) ))

testremove1 = TestCase (assertEqual "remove node without children" (Node 10 (Node 2 NIL NIL) NIL) (remove 20 (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL))))
testremove2 = TestCase (assertEqual "remove node that hasn't in tree" (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL)) (remove 90 (Node 10 (Node 2 NIL NIL) (Node 20 NIL NIL))))
testremove3 = TestCase (assertEqual "remove node with single children" (Node 10 (Node 2 NIL NIL) (Node 100 NIL NIL)) (remove 90 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testremove4 = TestCase (assertEqual "remove node with dual childrens (get successor's way)" (Node 90 (Node 2 NIL NIL) (Node 100 NIL NIL)) (remove 10 (Node 10 (Node 2 NIL NIL) (Node 90 NIL (Node 100 NIL NIL)))))
testremove5 = TestCase (assertEqual "remove node with data in repetition in tree and the first occurrence have single children" (Node 10 (Node 2 NIL NIL) (Node 110 NIL (Node 160 NIL NIL))) (remove 110 (Node 10 (Node 2 NIL NIL) (Node 110 NIL (Node 110 NIL (Node 160 NIL NIL))))))
testremove6 = TestCase (assertEqual "remove node with data in repetition in tree and the first occurrence have dual children" (Node 10 (Node 2 NIL NIL) (Node 110 (Node 80 NIL NIL) (Node 160 NIL NIL))) (remove 110 (Node 10 (Node 2 NIL NIL) (Node 110 (Node 80 NIL NIL) (Node 110 NIL (Node 160 NIL NIL))))))

testpreorder = TestCase (assertEqual "testpreorder" [3,2,1,4] (preOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testorder = TestCase (assertEqual "testorder" [1,2,3,4] (order (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testpostorder = TestCase (assertEqual "testpostorder" [1,2,4,3] (postOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))

testBST = TestList [testsize1,testsize2,testsize3,testisBST1,testisBST2,
 testisBST3,testinsert1,testinsert2,testinsert3,testsearch1,testsearch2,
 testsearch3,testpredecessor1,testpredecessor2,testsuccessor1,testsuccessor2,
 testremove1,testremove2,testremove3,testremove4,testpreorder,testorder,
 testpostorder,testmaximum1,testmaximum2,testminimum1,testminimum2,testsearch4,
 testinsert4,testremove5,testremove6]
