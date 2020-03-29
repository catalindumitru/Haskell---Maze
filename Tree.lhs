> module Tree(
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size

> )
> where

> import Geography

***************************************
*            Question 7               *
***************************************

> data Tree a = Empty | Fork (Tree a) a (Tree a) deriving Show

We make a balanced tree from a sorted list of places ps by dividing the list in two
halves and making each half of the list a balanced tree itself.

> makeBalanced :: Ord a => [a] -> Tree a                       -- for us a is Place
> makeBalanced [] = Empty
> makeBalanced ps = Fork (makeBalanced (take half ps)) x (makeBalanced left)  
>                      where half     = length ps `div` 2
>                            x : left = drop half ps

> bsearch :: Ord a => a -> Tree a -> Bool                      -- for us a is Place
> bsearch pl Empty                      = False
> bsearch pl (Fork t1 p t2) | pl == p   = True
>                           | pl < p    = bsearch pl t1
>                           | otherwise = bsearch pl t2

> data Maze = AMaze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)

We first construct a list of walls that consists of the walls from the
input list walls plus the boundary walls plus the reflected walls of
those. Then we sort this list and store it into walls', since places and 
directions can be ordered.

Then we divide walls' into 4 lists, each containing walls at some particular direction,
i.e. norths', souths', easts', wests'. For each of those, we make separate
lists containing the places of these walls, i.e. the first element of the pair.

Finally we convert these lists into the corresponding balanced trees.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = AMaze (x,y) north south east west
>    where 
>          north = makeBalanced norths
>          south = makeBalanced souths
>          east  = makeBalanced easts
>          west  = makeBalanced wests   

>          norths = map fst norths' 
>          souths = map fst souths'
>          easts  = map fst easts'
>          wests  = map fst wests'
                  
>          norths' = filter (\ x -> snd x == N) walls'
>          souths' = filter (\ x -> snd x == S) walls'
>          easts'  = filter (\ x -> snd x == E) walls'
>          wests'  = filter (\ x -> snd x == W) walls'

>          boundaries = [((i,y-1),N) | i <- [0..x-1]] ++ [((i,0),S) | i <- [0..x-1]] 
>                    ++ [((x-1,i),E) | i <- [0..y-1]] ++ [((0,i),W) | i <- [0..y-1]]

>          walls' = sort (walls ++ boundaries ++ map reflect (walls ++ boundaries))

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

> sort :: Ord a => [a] -> [a]
> sort [] = []
> sort (x:xs) = sort [a | a <- xs, a < x] ++ [x] ++ sort [a | a <- xs, a > x]

A maze has a wall at d in the place pl iff pl is an element of the tree of places that have 
a wall at d. This can be done by a binary search, since the trees are sorted as the lists
they come from were sorted as well.

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ north south east west) pl d | d == N = bsearch pl north
>                                              | d == S = bsearch pl south
>                                              | d == E = bsearch pl east
>                                              | d == W = bsearch pl west

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size 

Old representation (MyMaze.lhs) time and memory for largeMaze:

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,
E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,
W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,
S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,
W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,
E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.05 secs, 4,553,384 bytes)

New representation (Tree.lhs) time and memory for largeMaze:

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,
E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,
W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,
S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,
W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,
E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.05 secs, 7,775,320 bytes)

Old representation (MyMaze.lhs) time and memory for 100x100 maze:

*Main> fastSolveMaze m4 (0,0) (99,99)
* HUGE solution ommited *
(16.41 secs, 3,968,953,528 bytes)

New representation (Tree.lhs) time and memory for 100x100 maze:

*Main> fastSolveMaze m4 (0,0) (99,99)
* HUGE solution ommited *
(11.39 secs, 4,149,398,208 bytes)

We can notice a 5 seconds improvement and negligibly more memory usage.
