data TernaryTree = Empty | Node [Char] TernaryTree TernaryTree TernaryTree | NodeExists | NotReachable | NodeNotFound deriving (Show,Eq,Ord)

--Part One
charToInt::Char->Int
charToInt x = case x of '0' -> 0
                        '1' -> 1
                        '2' -> 2
                        '3' -> 3
                        '4' -> 4
                        '5' -> 5
                        '6' -> 6
                        '7' -> 7
                        '8' -> 8
                        '9' -> 9

--Part Two
insertNode::TernaryTree->[Char]->TernaryTree

createNode str = Node str Empty Empty Empty

numbers = "123"
numList x = [a | a <- x, a `elem` numbers]   --This function is used by insertNode and findNode functions

nodeList Empty = []
nodeList (Node x l m r) =  x : nodeList l ++ nodeList m ++ nodeList r

nodeNumList Empty = []
nodeNumList (Node x l m r) =  (numList x) : nodeNumList l ++ nodeNumList m ++ nodeNumList r

traverseTree (Node x Empty m r) str [y]
   | y == '1' = Node x (createNode str) m r
traverseTree (Node x Empty Empty r) str [y]
   | y == '1' = Node x (createNode str) Empty r
   | y == '2' = Node x Empty (createNode str) r
traverseTree (Node x Empty m Empty) str [y]
   | y == '1' = Node x (createNode str) m Empty
   | y == '3' = Node x Empty m (createNode str)
traverseTree (Node x l Empty r) str [y]
   | y == '2' = Node x l (createNode str) r
traverseTree (Node x l Empty Empty) str [y]
   | y == '2' = Node x l (createNode str) Empty
   | y == '3' = Node x l Empty (createNode str)
traverseTree (Node x l m Empty) str [y]
   | y == '3' = Node x l m (createNode str)
traverseTree (Node x l m r) str [y] = if y == '1' then Node x (createNode str) m r
                                      else if y == '2' then Node x l (createNode str) r
                                      else Node x l m (createNode str)
traverseTree (Node x l m r) str (y:ys) = if y == '1' then Node x (traverseTree l str ys) m r
                                         else if y == '2' then Node x l (traverseTree m str ys) r
                                         else Node x l m (traverseTree r str ys)

errorCases (Node x l m r) str
   | str `elem` (nodeList (Node x l m r)) = NodeExists
   | length (numList str) > 1 && (init (numList str)) `notElem` (nodeNumList (Node x l m r)) = NotReachable
   | length (numList str) > 1 && (init (numList str)) `elem` (nodeNumList (Node x l m r)) = traverseTree (Node x l m r) str (numList str)
   | length (numList str) == 1 = traverseTree (Node x l m r) str (numList str)

insertNode Empty str = if (numList str) /= [] then NotReachable
                       else createNode str
insertNode (Node x l m r) str = errorCases (Node x l m r) str

--Part Three
totalNodes::TernaryTree->Int
totalNodes Empty = 0
totalNodes (Node _ l m r) = 1 + totalNodes l + totalNodes m + totalNodes r

--Part Four
height::TernaryTree->Int
height Empty = 0
height (Node _ l m r) = 1 + max (height l) (max(height m) (height r))

--Part Five
levelcount::TernaryTree->Int->Int
nodecount Empty = 0
nodecount (Node _ l m r) = 1 + nodecount l + nodecount m + nodecount r

levelcount Empty _ = 0
levelcount (Node a l m r) 0 = nodecount (Node a l m r) - (nodecount l + nodecount m + nodecount r)
levelcount (Node _ l m r) level = levelcount l (level-1) + levelcount m (level-1) + levelcount r (level-1)

--Part Six
findNode::TernaryTree->[Char]->TernaryTree
removeFirst x = tail x

showTree Empty = Empty
showTree (Node x l m r) = Node x (showTree l) (showTree m) (showTree r)

searchTree Empty _ _ = NodeNotFound
searchTree (Node x l m r) str [] = if x == str then showTree (Node x l m r)
                                   else NodeNotFound
searchTree (Node x l m r) str (y:ys) = if y == '1' then searchTree l str ys
                                       else if y == '2' then searchTree m str ys
                                       else searchTree r str ys

findNode Empty _ = NodeNotFound
findNode (Node x l m r) str = if x == str then (Node x l m r)
                              else
                                 if length (numList str) > 1 then
                                    if numList str !! 0 == '1' then searchTree l str (removeFirst (numList str))
                                    else if numList str !! 0 == '2' then searchTree m str (removeFirst (numList str))
                                    else searchTree r str (removeFirst (numList str))
                                 else
                                    if numList str !! 0 == '1' then showTree l
                                    else if numList str !! 0 == '2' then showTree m
                                    else showTree r