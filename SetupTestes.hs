module SetupTestes
    (someTriple,
     someQuadruple,
     someTupleA,
     someTupleB,
     someTupleC,
     someTupleD,
     basicBST,
     emptyBST,
     unbalanceBST,
     littleBST,
     correctBST,
     incorrectBST1,
     incorrectBST2,
     bstWithRepetition,
     expectedInsert1,
     expectedInsert2,
     expectedInsert3,
     forSearch1,
     forSearch2,
     forSearch3,
     forSearch4,
     forSearch5,
     unbalancetoleft,
     unbalancetoright,
     minimumNode,
     maximumNode,
     expectedRemove1,
     expectedRemove2,
     expectedRemove3,
     expectedRemove4,
     bstWithRepetition1,
     expectedRemove5,
     bstWithRepetition2,
     emptyList,
     listWith6Elem,
     expectedTail1,
     expectedRemove6,
     expectedRemove7,
     forRemove
    ) where

import ProgFuncLista

someTriple = Triple (1::Int) "w" (5::Float)

someQuadruple = Quadruple "min" "gau" 19 60

someTupleA = TupleA "alfa"
someTupleB = TupleB "alfa" "bravo"
someTupleC = TupleC "alfa" "bravo" "charlie"
someTupleD = TupleD "alfa" "bravo" "charlie" "delta"

--List
emptyList = Nil
listWith6Elem = Cons 1 expectedTail1
expectedTail1 = (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil)))))

basicBST a b c d = Node 10 (Node 0 a b) (Node 20 c d)

--sizeBST
emptyBST = NIL
unbalanceBST = (Node 3 NIL (Node 4 NIL (Node 5 NIL (Node 9 (Node 7 NIL NIL) NIL))) )
littleBST = basicBST NIL NIL NIL NIL

--isBST
correctBST = basicBST NIL (Node 5 NIL NIL) (Node 15 NIL NIL) (Node 40 NIL NIL)
incorrectBST1 = basicBST NIL (Node 5 NIL NIL) (Node 7 NIL NIL) (Node 99 NIL NIL)
incorrectBST2 = basicBST NIL (Node 17 NIL NIL) NIL NIL

--insert
bstWithRepetition = basicBST NIL NIL NIL (Node 20 NIL NIL)
expectedInsert1 = basicBST NIL NIL (Node 17 NIL NIL) NIL
expectedInsert2 = basicBST NIL NIL NIL (Node 22 NIL NIL)
expectedInsert3 = basicBST NIL NIL NIL (Node 20 NIL (Node 20 NIL NIL))

--search
forSearch1 a = (Node 10 (Node 2 NIL NIL) a)
forSearch2 = forSearch1 forSearch3
forSearch3 = (Node 90 NIL (Node 100 NIL NIL))
forSearch4 = forSearch1 forSearch5
forSearch5 = (Node 110 NIL (Node 110 NIL NIL))

--maximum
unbalancetoright = (Node 5 NIL (Node 6 NIL (Node 7 NIL (Node 8 NIL maximumNode))) )
maximumNode = (Node 9 (Node 8.5 NIL NIL) NIL)

--minimum
unbalancetoleft = (Node 5 (Node 4 (Node 3 (Node 2 minimumNode NIL) NIL) NIL) NIL)
minimumNode = (Node 1 NIL (Node 1.5 NIL NIL))

--remove
expectedRemove1 = Node 10 (Node 0 NIL NIL) NIL
expectedRemove2 = forSearch1 (Node 100 NIL NIL)
expectedRemove3 = Node 90 (Node 2 NIL NIL) (Node 100 NIL NIL)
bstWithRepetition1 = Node 10 (Node 2 NIL NIL) (Node 110 NIL (Node 110 NIL (Node 160 NIL NIL)))
expectedRemove4 = Node 10 (Node 2 NIL NIL) (Node 110 NIL (Node 160 NIL NIL))
bstWithRepetition2 = Node 10 (Node 2 NIL NIL) (Node 110 (Node 80 NIL NIL) (Node 110 NIL (Node 160 NIL NIL)))
expectedRemove5 = Node 10 (Node 2 NIL NIL) (Node 110 (Node 80 NIL NIL) (Node 160 NIL NIL))
expectedRemove6 = (Node 10 NIL forSearch3)
forRemove = basicBST (Node (-1) (Node (-10) NIL NIL) NIL) NIL NIL NIL
expectedRemove7 = basicBST (Node (-10) NIL NIL) NIL NIL NIL
