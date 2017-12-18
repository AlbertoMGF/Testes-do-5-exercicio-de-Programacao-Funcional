module Testes
    (mytests
    ) where

import Prelude hiding (minimum,maximum,Just,Nothing,Maybe)
import ProgFuncLista
import Test.HUnit
import SetupTestes

--Triple
testtripleFst = TestCase (assertEqual "get first element of Triple"
 1 (tripleFst someTriple))
testtripleSnd = TestCase (assertEqual "get second element of Triple"
 "w" (tripleSnd someTriple))
testtripleThr = TestCase (assertEqual "get third element of Triple"
 5.0 (tripleThr someTriple))

--Quadruple
testquadrupleFirstTwo = TestCase (assertEqual "get first and second elements of Quadruple"
 ("min","gau") (firstTwo someQuadruple))
testquadrupleSecondTwo = TestCase (assertEqual "get third and fourty elements of Quadruple"
 (19,60) (secondTwo someQuadruple))

--Tuple
testTuple1A = TestCase (assertEqual "get first element of Tuple"
 (Just "alfa") (tuple1 someTupleA))
testTuple1B = TestCase (assertEqual "get first element of Tuple"
 (Just "alfa") (tuple1 someTupleB))
testTuple1C = TestCase (assertEqual "get first element of Tuple"
 (Just "alfa") (tuple1 someTupleC))
testTuple1D = TestCase (assertEqual "get first element of Tuple"
 (Just "alfa") (tuple1 someTupleD))

testTuple2A = TestCase (assertEqual "get second element of Tuple"
 (Nothing::Maybe [Char]) (tuple2 someTupleA))
testTuple2B = TestCase (assertEqual "get second element of Tuple"
 (Just "bravo") (tuple2 someTupleB))
testTuple2C = TestCase (assertEqual "get second element of Tuple"
 (Just "bravo") (tuple2 someTupleC))
testTuple2D = TestCase (assertEqual "get second element of Tuple"
 (Just "bravo") (tuple2 someTupleD))

testTuple3A = TestCase (assertEqual "get third element of Tuple"
 (Nothing::Maybe [Char]) (tuple3 someTupleA))
testTuple3B = TestCase (assertEqual "get third element of Tuple"
 (Nothing::Maybe [Char]) (tuple3 someTupleB))
testTuple3C = TestCase (assertEqual "get third element of Tuple"
 (Just "charlie") (tuple3 someTupleC))
testTuple3D = TestCase (assertEqual "get third element of Tuple"
 (Just "charlie") (tuple3 someTupleD))

testTuple4A = TestCase (assertEqual "get fourth element of Tuple"
 (Nothing::Maybe [Char]) (tuple4 someTupleA))
testTuple4B = TestCase (assertEqual "get fourth element of Tuple"
 (Nothing::Maybe [Char]) (tuple4 someTupleB))
testTuple4C = TestCase (assertEqual "get fourth element of Tuple"
 (Nothing::Maybe [Char]) (tuple4 someTupleC))
testTuple4D = TestCase (assertEqual "get fourth element of Tuple"
 (Just "delta") (tuple4 someTupleD))

--List
testLength1 = TestCase (assertEqual "size of empty List"
 0 (listLength emptyList))
testLength2 = TestCase (assertEqual "size of List with something"
 6 (listLength listWith6Elem))

testHead = TestCase (assertEqual "head of List with something"
 1 (listHead listWith6Elem))

testTail1 = TestCase (assertEqual "tail of List with something"
 expectedTail1 (listTail listWith6Elem))
testTail2 = TestCase (assertEqual "tail of empty List"
 (emptyList::List Int) (listTail emptyList))

testFoldr1 = TestCase (assertEqual "Foldr in empty List"
 5 (listFoldr (+) 5 emptyList))
testFoldr2 = TestCase (assertEqual "Foldr in List with something"
 26 (listFoldr (+) 5 listWith6Elem))
testFoldl1 = TestCase (assertEqual "Foldr in empty List"
 5 (listFoldl (+) 5 emptyList))
testFoldl2 = TestCase (assertEqual "Foldr in List with something"
 26 (listFoldl (+) 5 listWith6Elem))

--BST
testsize1 = TestCase (assertEqual "size of empty tree"
 0 (sizeBST emptyBST))
testsize2 = TestCase (assertEqual "size of very unbalanced tree"
 5 (sizeBST unbalanceBST))
testsize3 = TestCase (assertEqual "size of balanced tree"
 3 (sizeBST littleBST))

testisBST1 = TestCase (assertEqual "correct BST tree"
 True (isBST correctBST))
testisBST2 = TestCase (assertEqual "node with 7 in wrong position"
 False (isBST incorrectBST1))
testisBST3 = TestCase (assertEqual "node with 17 in wrong position"
 False (isBST incorrectBST2))
testisBST4 = TestCase (assertEqual "correct BST tree"
 True (isBST unbalanceBST))
testisBST5 = TestCase (assertEqual "correct BST tree"
 True (isBST (emptyBST::BinaryTree Int)))

testinsert1 = TestCase (assertEqual "insert in left position"
 expectedInsert1 (insert 17 littleBST ))
testinsert2 = TestCase (assertEqual "insert in right position"
 expectedInsert2 (insert 22 littleBST ))
testinsert3 = TestCase (assertEqual "insert in an empty tree"
 (Node 40 NIL NIL) (insert 40  emptyBST))
testinsert4 = TestCase (assertEqual "insert an element that has already in the tree"
 expectedInsert3 (insert 20  bstWithRepetition))

testsearch1 = TestCase (assertEqual "search tree's node "
 forSearch3 (search 90 forSearch2))
testsearch2 = TestCase (assertEqual "search tree's root"
 forSearch2 (search 10 forSearch2))
testsearch3 = TestCase (assertEqual "search for node that hasn't in tree"
 emptyBST (search 110 forSearch2))
testsearch4 = TestCase (assertEqual "search for node with data in repetition in tree"
 forSearch5 (search 110 forSearch4))

testmaximum1 = TestCase (assertEqual "search maximum without children"
 (Node 20 NIL NIL) (maximum littleBST))
testmaximum2 = TestCase (assertEqual "search maximum with children"
 maximumNode (maximum unbalancetoright))
testmaximum3 = TestCase (assertEqual "search maximum in emptyBST"
 emptyBST (maximum (emptyBST::BinaryTree Int)))

testminimum1 = TestCase (assertEqual "search minimum without children"
 (Node 0 NIL NIL)  (minimum littleBST))
testminimum2 = TestCase (assertEqual "search minimum with children"
 minimumNode  (minimum unbalancetoleft))
testminimum3 = TestCase (assertEqual "search minimum in emptyBST"
 emptyBST  (minimum (emptyBST::BinaryTree Int)))

testpredecessor1 = TestCase (assertEqual "predecessor in some lower position"
 2  (predecessor 3 unbalancetoleft))
testpredecessor2 = TestCase (assertEqual "predecessor in some higher position"
 90  (predecessor 100 forSearch2))

testsuccessor1 = TestCase (assertEqual "successor in some lower position"
 100  (successor 90 forSearch2))
testsuccessor2 = TestCase (assertEqual "successor in some higher position"
 3  (successor 2 unbalancetoleft))

testremove1 = TestCase (assertEqual "remove node without children"
 expectedRemove1 (remove 20 littleBST))
testremove2 = TestCase (assertEqual "remove node that hasn't in tree"
 littleBST (remove 90 littleBST))
testremove3 = TestCase (assertEqual "remove node with single children in right"
 expectedRemove2 (remove 90 forSearch2))
testremove4 = TestCase (assertEqual "remove node with dual childrens (get successor's way)"
 expectedRemove3 (remove 10 forSearch2))
testremove5 = TestCase (assertEqual "remove node with data in repetition in tree and the first occurrence have single children"
 expectedRemove4 (remove 110 bstWithRepetition1))
testremove6 = TestCase (assertEqual "remove node with data in repetition in tree and the first occurrence have dual children"
 expectedRemove5 (remove 110 bstWithRepetition2))
testremove7 = TestCase (assertEqual "remove node in left"
 expectedRemove6 (remove 2 forSearch2))
testremove8 = TestCase (assertEqual "remove node in emptyBST"
 emptyBST (remove 2 emptyBST))
testremove9 = TestCase (assertEqual "remove node with single children in left"
 expectedRemove7 (remove (-1) forRemove))

testpreorder = TestCase (assertEqual "testpreorder"
 [3,2,1,4] (preOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testpreorder1 = TestCase (assertEqual "testpreorder"
 [3,4,5,9,7] (preOrder unbalanceBST))
testorder = TestCase (assertEqual "testorder"
 [1,2,3,4] (order (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testorder1 = TestCase (assertEqual "testorder"
 [3,4,5,7,9] (order unbalanceBST))
testpostorder = TestCase (assertEqual "testpostorder"
 [1,2,4,3] (postOrder (Node 3 (Node 2 (Node 1 NIL NIL) NIL) (Node 4 NIL NIL))))
testpostorder1 = TestCase (assertEqual "testpostorder"
 [7,9,5,4,3] (postOrder unbalanceBST))

mytests = TestList [testFirstStructures,testBST]

testFirstStructures = TestList [testtripleFst,testtripleSnd,testtripleThr,
 testquadrupleFirstTwo,testquadrupleSecondTwo,testTuple1A,testTuple1B,
 testTuple1C,testTuple1D,testTuple2A,testTuple2B,testTuple2C,testTuple2D,
 testTuple3A,testTuple3B,testTuple3C,testTuple3D,testTuple4A,testTuple4B,
 testTuple4C,testTuple4D,testLength1,testLength2,testHead,testTail1,testTail2,
 testFoldr1,testFoldr2,testFoldl1,testFoldl2]

testBST = TestList [testsize1,testsize2,testsize3,testisBST1,testisBST2,
 testisBST3,testinsert1,testinsert2,testinsert3,testsearch1,testsearch2,
 testsearch3,testpredecessor1,testpredecessor2,testsuccessor1,testsuccessor2,
 testremove1,testremove2,testremove3,testremove4,testpreorder,testorder,
 testpostorder,testmaximum1,testmaximum2,testminimum1,testminimum2,testsearch4,
 testinsert4,testremove5,testremove6,testisBST4,testisBST5,testmaximum3,
 testminimum3,testremove7,testremove8,testremove9,testpreorder1,testorder1,
 testpostorder1]
