ibt(empty).
ibt(node(N,L,R)) :- integer(N) , ibt(L) , ibt(R).
/* Test cases for self testing*/
testibt(T) :- T = node(3 , node(4 , node(1,empty , empty) , empty ) , node( 2  , node(15 , empty , empty) , node(18 , empty , empty ))).
testibt2(T) :- T = node(3 , node(4 ,empty , empty ) , node( 2  , node(15 , empty , empty) , node(18 , node(21 , empty , empty), empty ))).
testbst(T) :- T =  node(7 , node(5  , node(4,empty,empty) , node(6,empty,empty) ) , node(  10  , node(8 , empty , empty) , node(18 , empty, empty ))).
ettest(T) :-  T = node( 1 , node( 2 , empty , node(4,empty,empty)) , node( 3 , node( 5 , empty , empty ) , node( 6 , node(7,empty,empty) , empty))).

/* size funciton, simple recursive formulation */
size(BT , N) :- BT = empty , N is 0.
size(BT , N) :- ibt(BT) , BT = node(_,L,R) , size(L, X) , size(R , Y) , N is X + Y + 1.

/* height funciton, simple recursive formulation */
height( BT , N) :- BT = empty , N is 0.
height(BT , N) :- ibt(BT) , BT = node(_,L,R) , height(L , X) , height(R,Y) , N is max(X,Y)+1.

/* inoder, funciton, simple recursive formulation , concatLists helper function to help concatenate answers*/
inorder( empty , []).
inorder(BT , L ) :- ibt(BT) , BT = node(Intlabel , Left , Right) , inorder(Left , X) , inorder(Right,Y) , concatLists( [X , [Intlabel] , Y], L).

/* preorder, funciton, simple recursive formulation , concatLists helper function to help concatenate answers*/
preorder( empty , [] ).
preorder(BT , L) :- ibt(BT) ,  BT = node(Intlabel , Left , Right) , preorder(Left , X) , preorder( Right , Y) ,concatLists( [[Intlabel] , X, Y], L).

/* postorder, funciton, simple recursive formulation , concatLists helper function to help concatenate answers*/
postorder(empty , []).
postorder( BT , L ) :-  ibt(BT) , BT = node(Intlabel , Left , Right) , postorder(Left , X) , postorder(Right , Y) , concatLists( [  X, Y, [Intlabel]], L).

trPreorder( BT , L) :- BT = empty , L = [] .
trPreorder( BT , L ) :- ibt(BT) ,  BT = node(_,_,_) , trPreorderHelper( BT , [] , [] , L).

/*trPreorderHelper( currentNode , NodeList , Accumulator , Ans) , NodeList has unprocessed elements , Node is processed and right child is added to the start of the umprocessed node list , the next node to be processed is the left node */
trPreorderHelper( empty , [] , A , L) :- L = A.
trPreorderHelper( empty , [X|Y] , A , L) :- trPreorderHelper( X , Y , A , L).
trPreorderHelper( Node , NL , A , L ) :- ibt(Node) , Node = node(Label,LT,RT) , append(A , [Label] , Acc) , trPreorderHelper( LT , [ RT| NL] , Acc , L).

trInorder(empty,[]).
trInorder(BT,L) :- BT = node(_,_,_) , trInorderHelper( BT , [] , [] , L).

/* trInorderHelper( current node , node list , accumulator , ans ) , when a node is processed the left node is next to be processed, the label is added to the unprocessed list,  ( when label is encountered later it is accumulated  ) and the right node is added to the front unprocessed list*/
trInorderHelper( empty , [] , L, L ).
trInorderHelper( empty , [X|Y] , A , L ):- trInorderHelper( X , Y , A , L).
trInorderHelper( NodeLabel , [X|Y] , A , L  ) :- integer(NodeLabel) , append( A , [NodeLabel] , Acc ) , trInorderHelper(X , Y , Acc , L).
trInorderHelper(Node , NL , A , L ) :- Node = node(N,LT,RT) , append( [N] , [RT] , Temp) ,append(Temp , NL , Temp2) ,  trInorderHelper( LT ,Temp2, A, L).

trPostorder(BT,L) :- BT = empty , L = [] .
trPostorder( BT , L ) :- BT = node(_,_,_) , trPostorderHelper( BT , [] , [] , L).

/* trPostOrderHelper( current node , node list , accumulator , ans ) , when a node is processed it is appended to the front of the list(after recursion it appears at the end ), the R child is the
next node to be processed and Left child is added to the node list , whenever we accumulate result , we add it to the front of the list ( giving post order)   */
trPostorderHelper( empty , [] , A ,  A ).
trPostorderHelper( empty , [X|Y] , A , L ):- trPostorderHelper(X , Y , A , L).
trPostorderHelper( Node , NL , A , L ):- Node = node(Label,LT,RT) , append([Label] , A , Acc) , trPostorderHelper( RT , [LT|NL] , Acc , L ).

/* Helper function to concat a list of strings into a single string*/
concatStringList([] , "").
concatStringList([X|Y] , L) :- concatStringList(Y , Temp) , string_concat(X , Temp , L).

toString(empty , "()").
toString( BT , S) :-  BT = node(N , Left , Right) , toString(Left , X) , toString(Right , Y) , number_string(N , NtoNumber) ,  concatStringList(["(" , NtoNumber , ", " , X , ", "  , Y , ")"] , S).

isBalanced( empty ).
isBalanced(BT) :- BT = node( _ , LT , RT) , isBalanced(LT) , isBalanced(RT) , height(LT , X) , height(RT , Y) , Yplusone is Y+1 , Xplusone is X+1 ,  Y =< Xplusone , X =< Yplusone.
/* helper function for make bst for empty list -1000 is used as a base case*/
maxOfTree(empty , -1000).
maxOfTree( node(X , empty , empty) , X ) :- integer(X).
maxOfTree( node(X , node(Y,empty,empty)) , empty , N) :- N is max(X,Y).
maxOfTree( node(X ) , empty , node(Y , empty , empty) , N) :- N is max(Y,X).
maxOfTree(BT , A) :- BT = node(N , LT , RT) , maxOfTree( LT , X) , maxOfTree(RT , Y) , Z is max(X,Y) , A is max(N,Z).
/* helper function for make bst for empty list 1000 is used as a base case*/
minOfTree(empty , 1000).
minOfTree( node(X , empty , empty) , X ) :- integer(X).
minOfTree( node(X , node(Y,empty,empty)) , empty , N) :- N is min(X,Y).
minOfTree( node(X ) , empty , node(Y , empty , empty) , N) :- N is min(Y,X).
minOfTree(BT , A) :- BT = node(N , LT , RT) , minOfTree( LT , X) , minOfTree(RT , Y) , Z is min(X,Y) , A is min(N,Z).


isBST( empty ).
isBST( BT ) :- ibt(BT) ,  BT = node(N , LT , RT) , isBST(LT) , isBST(RT) , minOfTree(RT , Y) , maxOfTree(LT , X) , N < Y , X < N .

/* helper function to concat a list of lists into single list */ 
concatLists( [] , [] ).
concatLists( [X|Y] , Ans ) :- concatLists( Y , Ans1) , append( X , Ans1 , Ans).

eulerTour( empty, []).
eulerTour( BT , L ) :- BT = node( N , LT , RT ) , eulerTour( LT , X) , eulerTour( RT , Y ) , concatLists([ [N] , X , [N] , Y , [N]] , L).

/* if the root is the element then we get true , otherwise we check in the right and left part , lookup in empty tree will be automatically false  */
lookup( N , BST ) :- BST = node( N , _ , _ ).
lookup( N , BST ) :- BST = node(  X , L , _ ) , N < X , lookup( N , L ) .
lookup( N , BST ) :- BST = node( X , _ , R ) , N > X , lookup( N , R).

/* if the root is the element to be inserted then, don't insert otherwise insert in left subtree or right subtree accordingly */

insert( N , BST1 , BST2) :- BST1 = empty , BST2 = node(N,empty , empty).
insert( N , BST1 , BST2 ) :- BST1 = node(X,L,R) , N < X , insert( N , L , Temp) , BST2 = node( X, Temp,R).
insert( N , BST1 , BST2 ) :- BST1 = node( X , L , R ) , N >X , insert( N, R , Temp) , BST2 = node( X , L , Temp).
insert( N , BST1 , BST2 ) :- BST1 = node(X,L,R) , X == N ,  BST2 = node(N,L,R).

/*
IF the tree is empty, then no need to delete,
    if only root in tree, delete it ,
    delete in the left / right tree if needed
    if root is to be deleted, and any of the subtrees is empty return
    the other subtree
    otherwise delete the max element in the left and make it the new root*/

delete(_, BST1 , BST2) :- BST1 = empty , BST2 = empty.
delete( N , BST1 , BST2 ):- BST1 = node( N , empty , empty ) , BST2 = empty.
delete(N , BST1 ,BST2 ) :- BST1 = node( X , L , R )  , X < N , delete( N , R , Temp) , BST2 = node(X , L , Temp).
delete( N , BST1 , BST2 ) :- BST1 = node( X, L, R ) , N < X , delete( N , L , Temp) , BST2 = node(X , Temp , R).
delete( N , BST1 , BST2 ) :- BST1 = node( N , L , empty ) , BST2 = L.
delete( N , BST1 , BST2 ) :- BST1 = node(N,empty , R) , BST2 = R.
delete(N , BST1 , BST2 ) :- BST1 = node(N , L , R ) , maxOfTree( L , Temp) , delete(Temp , L , Temp2) , BST2 = node( Temp , Temp2 , R).

makeBST( L , T ) :- sort( L , LDash) , makeBSTHelper( LDash , T ) . /* we sort the list, and remove the duplicates from the list because in presence of duplicates it is not possible to make a balanced tree */

makeBSTHelper( [] , T ) :- T = empty.
makeBSTHelper([X] , T ) :-  T = node(X  , empty , empty ).
makeBSTHelper( L , T   ) :- segList( L , [] , X , Y , Z) , makeBSTHelper( X , LT) , makeBSTHelper( Z , RT ) , T = node(Y , LT , RT).

/*segList( L , FirstAcc , First , Int , Last ).
used for make bst function to divide the list in two almost equal parts and the root for the BST( middle elements )*/

segList( [X|Y] , L1 , F , N , S) :- length(Y , Temp1) , length(L1 , Temp2 ) , Temp1 = Temp2 , F = L1 , N = X , S = Y.
segList( [X|Y] , L1 , F , N , S ) :- length(Y,Temp1) , length(L1 , Temp2 ) , Temp2PlusOne is Temp2 +1 , Temp2PlusOne = Temp1 , F = L1 , N = X , S = Y.
segList([X|Y] , L1 , F , N , S ) :- append( L1 , [X] , Temp) , segList( Y , Temp , F , N , S).

preET( empty , [] ).
preET( BT , L ) :- eulerTour( BT , ET ) , preOrderFromET( ET , L ).

/* We divide the euler tour such that  L =[[X] | L1 | [X] | L2 | [X]] , and recurse on L1 and L2,  prolog will find all the solutions by backtracking
the answers are concatenaed differently depeding on the type of the traversal*/

preOrderFromET( ET , L ) :- ET = [] , L = [] .
preOrderFromET( [X|Y] , L ) :- divideListHelper( X , Y , [] , L1 , L2)  , divideListHelper( X , L2 , [] , L3 , L4 ) , L4 = [] ,  preOrderFromET( L1 , Temp1) , preOrderFromET( L3 , Temp2 ) ,concatLists( [[X] , Temp1 , Temp2] , L) .

postET( empty , [] ) .
postET( BT , L ) :-  eulerTour( BT , ET ) , postOrderFromET( ET , L ) .

postOrderFromET( ET , L ) :- ET = [] , L = [] .
postOrderFromET( [X|Y] , L ) :- divideListHelper( X , Y , [] , L1 , L2) , divideListHelper( X , L2 , [] , L3 , L4 ) , L4 = [] , postOrderFromET( L1 , Temp1) , postOrderFromET( L3 , Temp2 ) ,concatLists( [  Temp1 , Temp2 , [X]] , L) .


inET( empty , [] ) .
inET( BT , L ) :- eulerTour( BT,ET) , inOrderFromET( ET , L ) .

inOrderFromET( ET , L ) :- ET = [] , L = [] .
inOrderFromET( [X|Y] , L ) :- divideListHelper( X , Y , [] , L1 , L2) , divideListHelper( X , L2 , [] , L3 , L4 ) , L4 = [] ,  inOrderFromET( L1 , Temp1) , inOrderFromET( L3 , Temp2 ) ,concatLists( [  Temp1 , [X] ,  Temp2 ] , L) .

 /*divideListHelperET( X , L , T , L1 , L2 ) used for dividing the euler tour to extract tree traversals X, is the root of the tree L1 is the list of elements before the  occurence of element X in list L , L2 are the elements after that occurence of X in list L , T is an accumulator for L1  
 by backtracking all solutions are generated*/

divideListHelper( X , [Z|Y] , Acc ,  L1 , L2 ) :-  append( Acc , [Z] , Accnew) ,  divideListHelper( X , Y , Accnew , L1 , L2 ) .
divideListHelper( X , [X|Z] , Acc,  L , Y) :- L = Acc , Z = Y.



