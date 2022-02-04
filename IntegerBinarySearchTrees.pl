ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).


size(empty,0). %Base Case that size of empty tree is 0
size(BT,N):- BT=node(_,L,R), size(L,A), size(R,B), N is 1+A+B. % Recursively calling the sizes of the Left and Right Subtrees of the Node


%max(A,B,X) returns the maximum value among A and B.
max(A,B,A):- A>=B.
max(A,B,B):-A<B.

height(empty,0). %Base Case that height of empty tree is 0
height(BT,H):- BT = node(_,L,R), height(L,X),height(R,Y),max(X,Y,C),H is 1+C. %Height of the Tree is 1 + max(Height of Left Subtree,Height of Right Subtree).


preorder(empty,[]). %Base Case that preorder of an empty tree is a null list
preorder(BT,L):- BT = node(N,LT,RT),preorder(LT,Lft),preorder(RT,Rft),X = [N|Lft],append(X,Rft,L).
/*Recursively calling the preorder lists of Left and Right Subtrees, appending them together and inserting the value to the front of the list.*/


inorder(empty,[]). %Base Case that inorder of an empty tree is a null list
inorder(BT,L):- BT = node(N,LT,RT), inorder(LT,Lft), append(Lft,[N],X),inorder(RT,Rft),append(X,Rft,L).
/*Recursively calling the inorder lists of Left and Right Subtrees, inserting the value to the left subtree inorder list and appending to the right subtree inorder list*/


postorder(empty,[]). %Base Case that postorder of an empty tree is a null list
postorder(BT,L):- BT= node(N,LT,RT), postorder(LT,Lft),postorder(RT,Rft),append(Lft,Rft,X),append(X,[N],L).
/*Recursively calling the preorder lists of Left and Right Subtrees, appending them together and inserting the value to the end of the list.*/


/*Tail Recursive Preorder:
We take a stack of Nodes, initially containing the RootNode, Implemented through lists.
If the stack is empty, Our answer list is the accumulator list.
If the top is an empty node, we recursively call our function on the popped stack.
Else, we pop the top of the stack, push the Rightnode then Leftnode into it, append the value of top to end of our accumulator List and call our function on the new stack after the operations.
*/
trPreorder(BT,L):- X = [BT], prHelper(X,[],L).
prHelper([],L,L).
prHelper([Head|Tail],X,L):- Head = empty, prHelper(Tail,X,L).
prHelper([Head|Tail],X,L):- Head = node(N,LT,RT), append(X,[N],P), Y = [RT|Tail],Z = [LT|Y],prHelper(Z,P,L).


/*Tail Recursive Inorder:
We maintain a stack of Nodes, Initially containing the RootNode,Implemented through lists.
If the stack is empty, Our answer list is the accumulator list.
If the top is a Leaf, we pop it out, append it's value to the end Accumulator List and call the function on the popped stack.
If the top is a node without a Left Child,we pop it out and append it's value to the end of Accumulator list, push the Right child to the stack and call the function on the pushed stack.
If the top has a Left Child and if the value of the Left Child is there in our Accumulator List, then we pop the top,append it's value to end of our accumulator list and push the right child to the stack, and call the function on the new stack after operations.
If the top has a Left Child and if the value of the Left Child is not there in our Accumulator List, then we push the left child to the stack and call the function on it.
*/
trInorder(BT,L):- X = [BT], inHelper(X,[],L).
inHelper([],L,L).
inHelper(List,X,L):- List = [Head|Tail], Head = node(N,empty,empty), append(X,[N],M),inHelper(Tail,M,L).
inHelper(List,X,L):- List = [Head|Tail], Head = node(N,LT,empty),LT = node(M,_,_), member(M,X), append(X,[N],P), inHelper(Tail,P,L).
inHelper(List,X,L):- List = [Head|_], Head = node(_,LT,empty),inHelper([LT|List],X,L).
inHelper(List,X,L):- List = [Head|Tail], Head = node(N,empty,RT), append(X,[N],M), inHelper([RT|Tail],M,L).
inHelper(List,X,L):- List = [Head|Tail], Head = node(N,LT,RT), LT = node(M,_,_), member(M,X), append(X,[N],P), inHelper([RT|Tail],P,L).
inHelper(List,X,L):- List = [Head|_], Head = node(_,LT,_), inHelper([LT|List],X,L).


/*Tail Recursive Postorder:
We take a stack of Nodes, initially containing the RootNode, Implemented through lists.
If the stack is empty, Our answer list is the accumulator list.
If the top is an empty node, we recursively call our function on the popped stack.
Else, we pop the top of the stack, push the LeftNode then RightNode into it, append the value of top to front of our accumulator List and call our function on the new stack after the operations.
*/
trPostorder(BT,L):- X = [BT], pstHelper(X,[],L).
pstHelper([],L,L).
pstHelper([Head|Tail],X,L):- Head = empty, pstHelper(Tail,X,L).
pstHelper([Head|Tail],X,L):- Head = node(N,LT,RT), P = [N|X], Y = [LT|Tail], Z = [RT|Y], pstHelper(Z,P,L).


/*EulerTour:
Euler Tour of an empty node is the null list.
Euler Tour of an isolated vertex would be the value 3 times in a list.
When the left child is empty, Euler tour would be the value twice appended at the front and once at the end of Euler tour of right subtree.
When the right child is empty, Euler tour would be the value twice appended at the end and once at the front of Euler tour of left subtree.
*/
eulerTour(empty,[]).
eulerTour(BT,[N,N,N]):- BT = node(N,empty,empty).
eulerTour(BT,L):- BT = node(N,empty,RT), eulerTour(RT,X), Z = [N|X],W = [N|Z],append(W,[N],L).
eulerTour(BT,L):- BT = node(N,LT,empty), eulerTour(LT,X),Z = [N|X],append(Z,[N],W),append(W,[N],L).
eulerTour(BT,L):- BT = node(N,LT,RT), eulerTour(LT,X), eulerTour(RT,Y),Z = [N|Y], append(Z,[N],W), append(X,W,U), L = [N|U].


/*Preorder from the Euler Tour of the BT:
The Preorder would be the order of first occurences of elements.
For this, we call our Helper Function on the Euler Tour.
Our Helper Function returns the Accumulator list as Answer list when the Input list becomes empty.
Otherwise it takes out the head of the list, if it is already present in our Accumulator List, we just call the Function on the Tail keeping accumulator same.
If the head is not present in our Accumulator List, we append it to the end of the Accumulator List.
*/
preET(BT,L):- eulerTour(BT,X), pEThelper(X,[],L).
pEThelper([],L,L).
pEThelper([Head|Tail],X,L):- member(Head,X), pEThelper(Tail,X,L).
pEThelper([Head|Tail],X,L):- append(X,[Head],P),pEThelper(Tail,P,L).


/*Inorder from the Euler Tour of the BT:
The Inorder would be the order of second occurences of elements.
For this, we maintain a list which contains every element which has been visited currently.
If the head is present in this list and is also present in the tail of current list, then it is the second occurence of that value and hence is appended to the end of our accumulator list.
Else we insert the head to the visited list and call the function on the tail and the new visited list.
*/
inET(BT,L):- eulerTour(BT,X), inEThelper(X,[],[],L).
inEThelper([],_,L,L).
inEThelper([Head|Tail],X,M,L):- member(Head,X), member(Head,Tail), append(M,[Head],P), inEThelper(Tail,X,P,L).
inEThelper([Head|Tail],X,M,L):- Y = [Head|X], inEThelper(Tail,Y,M,L).


/*Postorder from the Euler Tour of the BT:
The Postorder would be order of the last occurences of elements.
If the head is not present in the Tail of the current list, then it is the last occurence and hence appended to the end of our accumulator list.
Else we call our function on the tail of the current list with the same accumulator list.
*/
postET(BT,L):- eulerTour(BT,X),postEThelper(X,[],L).
postEThelper([],L,L).
postEThelper([Head|Tail],X,L):- member(Head,Tail), postEThelper(Tail,X,L).
postEThelper([Head|Tail],X,L):- append(X,[Head],P), postEThelper(Tail,P,L).


/*toString:
toString for an empty BT would be "()".
Otherwise we convert the value of node to string, concat "(" with the value, concat the toStrings of LT and RT and concat ")".
*/
toString(empty,"()").
toString(BT,S):- BT = node(N,LT,RT), number_string(N,S1),toString(LT,S2),toString(RT,S3),string_concat("(",S1,S4), string_concat(S4,S2,S5),string_concat(S5,S3,S6),string_concat(S6,")",S).


/*iSBalanced:
An empty BT is balanced->Axiom.
For the BT to be balanced, the height of the left and right subtrees should differ by at max 1, and the Left and Right subtrees should be balanced.
*/
isBalanced(empty).
isBalanced(BT):- BT = node(_,LT,RT), isBalanced(LT),isBalanced(RT),height(LT,Lh),height(RT,Rh),Lh<Rh+2, Lh>Rh-2.


/*isBST:
An empty tree is a BST->Axiom.
An isolated vertex is a BST->Axiom.
Otherwise, the left and right subtrees should be a BST and the value of left child should be <= value of node < value of right child.
*/
isBST(empty).
isBST(BT):- BT = node(_,empty,empty).
isBST(BT):- BT = node(N,LT,empty), LT = node(N1,_,_), N>=N1,isBST(LT).
isBST(BT):- BT = node(N,empty,RT), RT = node(N2,_,_), N2>N, isBST(RT).
isBST(BT):- BT = node(N,LT,RT), LT = node(N1,_,_), RT = node(N2,_,_),N>=N1,N2>N,isBST(LT),isBST(RT).

/*makeBST:
The helper function split, splits a List L into 2 lists X and Y from the middle, with length of X<= length of Y.
For making a BST from a List, we first sort it, make the value of middle(the head of Y) the rootnode value and make a bst from X as Left subtree and the bst from Tail as the Right subtree.
*/
split(L,X,Y):- length(L,N), N0 is N div 2, length(X,N0), append(X,Y,L).
makeBST([],empty).
makeBST(L,BST):-sort(L,M), split(M,X,Y), Y = [Head|Tail], makeBST(X,LST),makeBST(Tail,RST), BST = node(Head,LST,RST).


/*lookup:
If the rootnode has the value N, we return true.
Else if N< value of rootnode, we lookup in the left subtree.
Else we lookup in the right subtree.
*/
lookup(N,BST):- BST = node(N,_,_).
lookup(N,BST):- BST = node(X,LT,_),X>N, lookup(N,LT).
lookup(N,BST):- BST = node(X,_,RT),N>X, lookup(N,RT).


/*insert:
If the BST1 is empty, we create an isolated vertex with value N as our BST2.
Else if N is less than rootnode value, we insert the node to the left subtree.
Else we insert the node to the right subtree.
*/
insert(N,empty,node(N,empty,empty)).
insert(N,BST1,BST2):- BST1 = node(N1,LT1,RT1), BST2 = node(N1,LT2,RT1),N<N1,insert(N,LT1,LT2).
insert(N,BST1,BST2):- BST1 = node(N1,LT1,RT1), BST2 = node(N1,LT1,RT2),N>N1,insert(N,RT1,RT2).


/*delete:
If the Label to be deleted is present in a leaf node, we make that node empty(i.e. delete it).
If the Label to be deleted is present in the left subtree, we make the right subtree and node value of BST2 same and call for delete function on left subtree.
If the Label to be deleted is present in the left subtree, we make the right subtree and node value of BST2 same and call for delete function on left subtree.
If the Node containing the label has only a left or right subtree, we remove the node and push the respective subtree in place of it.
Otherwise, we find the inorder successor of the node containing the label, copy the inorder successor to the node and delete the successor.
*/
delete(N,node(N,empty,empty),empty).
delete(N,BST1,BST2):- BST1 = node(N1,LT1,RT1), BST2 = node(N1,LT2,RT1), N<N1, delete(N,LT1,LT2).
delete(N,BST1,BST2):- BST1 = node(N1,LT1,RT1), BST2 = node(N1,LT1,RT2), N>N1, delete(N,RT1,RT2).
delete(N,BST1,BST2):- BST1 = node(N,empty,RT1), BST2 = RT1.
delete(N,BST1,BST2):- BST1 = node(N,LT1,empty), BST2 = LT1.
delete(N,BST1,BST2):- BST1 = node(N,_,_),inorderSucc(BST1,X),X = node(M,_,_), delete(M,BST1,Y), Y = node(_,Q,R), BST2 = node(M,Q,R).

%Helper function inorderSucc returns the inorder successor node of a given node 
inorderSucc(node(_,_,RT),X):- leastval(RT,X). %The inorder successor is the least value in the right subtree.
leastval(BT,BT):- BT = node(_,empty,_).
leastval(BT,X):- BT = node(_,LT,_), leastval(LT,X).





























