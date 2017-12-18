module ProgFuncLista
    ( Maybe(Just,Nothing),
      BinaryTree(Node,NIL),
      sizeBST,
      isBST,
      insert,
      search,
      maximum,
      minimum,
      predecessor,
      successor,
      remove,
      preOrder,
      order,
      postOrder,
      matricula,
      Triple(Triple),
      tripleFst,
      tripleSnd,
      tripleThr,
      Quadruple(Quadruple),
      firstTwo,
      secondTwo,
      Tuple(TupleA,TupleB,TupleC,TupleD),
      tuple1,
      tuple2,
      tuple3,
      tuple4
    ) where

import Prelude hiding (minimum,maximum,Just,Nothing,Maybe)

--Digite abaixo a sua matricula
matricula = 123456789 :: Int
--Digite acima a sua matricula

data Maybe x = Just x | Nothing  deriving (Eq,Show)
--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = TupleA a | TupleB a b | TupleC a b c | TupleD a b c d  deriving (Eq,Show)

tuple1 (TupleA a) = Just a
tuple1 (TupleB a b) = Just a
tuple1 (TupleC a b c) = Just a
tuple1 (TupleD a b c d) = Just a

tuple2 (TupleA a) = Nothing
tuple2 (TupleB a b) = Just b
tuple2 (TupleC a b c) = Just b
tuple2 (TupleD a b c d) = Just b

tuple3 (TupleA a) = Nothing
tuple3 (TupleB a b) = Nothing
tuple3 (TupleC a b c) = Just c
tuple3 (TupleD a b c d) = Just c

tuple4 (TupleA a) = Nothing
tuple4 (TupleB a b) = Nothing
tuple4 (TupleC a b c) = Nothing
tuple4 (TupleD a b c d) = Just d

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a NIL NIL) = True
isBST (Node a (Node b left right) NIL) = b <= a && isBST (Node b left right)
isBST (Node a NIL (Node b left right)) = b >= a && isBST (Node b left right)
isBST (Node a (Node b left right) (Node c lleft rright)) = b <= a && a <= c && isBST (Node b left right) && isBST (Node c lleft rright)

--insere uma nova chave na BST retornando a BST modificada
insert x NIL = (Node x) NIL NIL
insert x (Node a (left) (right)) = if x < a
  then Node a (insert x left) (right)
  else Node a (left) (insert x right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a (left) (right)) = if x == a
  then (Node a (left)(right))
  else (if x < a
    then search x (left)
    else search x (right))

--retorna no com elmento maximo da BST
maximum NIL = NIL
maximum (Node a left NIL) = Node a left NIL
maximum (Node a left right)= maximum right

--retorna no com elemento minimo da BST
minimum NIL = NIL
minimum (Node a NIL right) = (Node a NIL right)
minimum (Node a left right)= minimum left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor x (Node a left right) = if search x (Node a left right) /= NIL
  then (order (Node a left right) !! (position x (Node a left right) - 1))
  else x
--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST

successor x (Node a left right) = if search x (Node a left right) /= NIL
  then (order (Node a left right) !! (position x (Node a left right) + 1))
  else x

--Remove um dado elemento
remove x NIL = NIL
remove x (Node a left right) = if x == a
  then remove_action x (Node a left right)
  else (if x < a
    then Node a (remove x (left)) right
    else Node a left (remove x (right)))
--remove ume lemento da BST
remove_action x NIL = NIL
--elemento eh folha
remove_action x (Node b NIL NIL) = NIL
--elemento tem um filho
remove_action x (Node b left NIL) = left

remove_action x (Node b NIL right) = right
--elemento tem dois filhos
remove_action x (Node b left right) = Node (successor b (Node b NIL right)) left (remove (successor b (Node b NIL right)) right)

--retorna uma lista de nos com os dados da BST nos diversos tipos de caminhamento
preOrder (Node a NIL NIL) = [getDate (Node a NIL NIL)]
preOrder (Node a left NIL) = [getDate (Node a NIL NIL)] ++ preOrder left
preOrder (Node a NIL right) = [getDate (Node a NIL NIL)] ++ preOrder right
preOrder (Node a left right) = ([getDate (Node a NIL NIL)] ++ preOrder left) ++ preOrder right

order (Node a NIL NIL) = [getDate (Node a NIL NIL)]
order (Node a left NIL) = order left ++ [getDate (Node a NIL NIL)]
order (Node a NIL right) = [getDate (Node a NIL NIL)] ++ order right
order (Node a left right) = (order left ++ [getDate (Node a NIL NIL)]) ++ order right

postOrder (Node a NIL NIL) = [getDate (Node a NIL NIL)]
postOrder (Node a left NIL) = postOrder left ++ [getDate (Node a NIL NIL)]
postOrder (Node a NIL right) = postOrder right ++ [getDate (Node a NIL NIL)]
postOrder (Node a left right) = (postOrder left ++ postOrder right) ++ [getDate (Node a NIL NIL)]

getDate (Node a left right) = a

--retorna No com dado de acordo com a sua posição
position x NIL = -1
position x (Node a left right) = if search x (Node a left right) == NIL
  then -1
  else position_search x (order (Node a left right)) 0

position_search x [] p = -1
position_search x (a : xs) p = if x == a
  then p
  else position_search x (xs) (p + 1)
