> import Geography

 import MyMaze

 import Maze

> import Tree

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

We will store the maze as a list of Strings, where each String is a row.
If the row is odd, it is of type +--+, otheriwse of type | |.
We number the rows from 1 to 2*y+1.
For an odd row r we apply the function drawEW maze r x to draw E-W oriented walls.
This looks at the (i, r/2) cells of the maze and checks for S walls, 0 <= i <= x-1
For an even row r we apply the function drawNS maze r x to draw N-S oriented walls.
This looks at the (i, (r-1)/2) cells of the maze and checks for E walls, 0 <= i <= x-1

> drawMaze :: Maze -> IO()
> drawMaze maze = (putStr . concat) [if r `mod` 2 == 0
>                                    then drawNS maze r x
>                                    else drawEW maze r x
>                                    | r <- [2*y+1,2*y .. 1]]

>                 where (x,y) = sizeOf maze

> drawEW maze r x = "+" ++ concat [(if hasWall maze (i,q) S
>                                   then "--" 
>                                   else "  ") ++ "+" | i <- [0 .. x-1]]
>                       ++ "\n"
>                  where q = r `div` 2

> drawNS maze r x = "|" ++ concat ["  " ++ if hasWall maze (i,q) E 
>                                          then "|"
>                                          else " " | i <- [0 .. x-1]]
>                       ++ "\n"
>                  where q = (r-1) `div` 2                     

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze start target = solveMazeIter maze target [(start, [])]

> fastSolveMaze :: Maze -> Place -> Place -> Path
> fastSolveMaze maze start target = fastSolveMazeIter maze [] target [(start, [])]

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze target []                       = error "Impossible to reach" 
> solveMazeIter maze target (x:tail) | pos == target = path
>                                    | otherwise     = solveMazeIter maze target
>                                     (tail ++ concat
>                                     [if not (hasWall maze pos d) && 
>                                         (path == [] || last path /= opposite d) 
>                                      then [(move d pos, path ++ [d])] 
>                                      else [] | d <- [N,S,E,W]])

>                                     where (pos,path) = x

(x:tail) is the list of pairs (Place, Path) that are left to be checked at the current time.
If it is empty, this means that the target has not been found and so it cannot be reached.
If the first pair in the list is (target, path) then we return path, i.e. the path to target.
Otherwise, we add the 4 possible directions to go from that position to the tail of our list.
We make sure that we do not add to the tail of the list a pair (pos,path) that would return
to the position we came from.

> fastSolveMazeIter :: Maze -> [Place]-> Place -> [(Place, Path)] -> Path
> fastSolveMazeIter maze visited target list | list == []    = error "Impossible to reach"
>                                            | pos == target = path
>                                            | otherwise     = fastSolveMazeIter 
>                                              maze (pos:visited) target
>                                             (tail list ++ concat
>                                             [if not (hasWall maze pos d) && 
>                                                 not (move d pos `elem` visited)
>                                              then [(move d pos, path ++ [d])] 
>                                              else [] | d <- [N,S,E,W]]) 

>                                             where (pos,path) = head list

This is the same function as above with the improvement that it does not add to the tail of
the list positions that have already been visited.

======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls

***************************************
*            Question 6               *
***************************************

verticalHoles i y ys creates a list of E walls in the places (i,j) where 0 <= j <= y-1 and 
j is not in ys (i.e. hole at (i,j) at East if j is in ys)

horizontalHoles j x xs creates a list of N walls in the places (i,j) where 0 <= i <= x-1 and
i is not in xs (i.e. hole at (i,j) at North if i is in xs)

maze1 x y xss yss creates a maze of size (x,y) with walls such that the maze has 
vertical holes on column i at (yss!!i) and horizontal holes on row j at (xss!!j).
Note that 0 <= i <= x-2 because the eastern boundary walls cannot have holes and
          0 <= j <= y-2 because the northern boundary walls cannot have holes.

> verticalHoles, horizontalHoles :: Int -> Int -> [Int] -> [((Int,Int),Direction)]

> verticalHoles i y ys = [((i,j),E) | j <- [0..y-1], not (j `elem` ys)]
> horizontalHoles j x xs = [((i,j),N) | i <- [0..x-1], not (i `elem` xs)]

> maze1 x y xss yss = makeMaze (x,y) walls
>                        where walls = concat [verticalHoles i y (yss!!i) | i <- [0..x-2]] ++
>                                      concat [horizontalHoles j x (xss!!j) | j <- [0..y-2]]
>                                      

> m1 = maze1 5 5 [[0,1,2,3],[2,4],[1],[0,1,4]] [[1,2,3],[0,2],[0],[1]]

> maze2 x y o | o == "Horizontal" = f $ concat [if j `mod` 2 == 0 
>                                               then [((i,j),N) | i <- [0..x-2]] 
>                                               else [((i,j),N) | i <- [1..x-1]]  
>                                               | j <- [0..y-1]] 
>             | o == "Vertical"   = f $ concat [if i `mod` 2 == 0 
>                                               then [((i,j),E) | j <- [0..y-2]]
>                                               else [((i,j),E) | j <- [1..y-1]] 
>                                               | i <- [0..x-1]] 
>             where f = makeMaze (x,y)

> m2 = maze2 5 3 "Vertical"

> maze3 x y o lim = makeMaze (x,y) walls 
>                 where walls | o == "Horizontal" = concat [if j `mod` 2 == 0 
>                                                           then [((i,j),N) | i <- [0..lim-1]]
>                                                           else [((i,j),N) | i <- [1..lim]]  
>                                                           | j <- [0..y-1]] ++
>                                                   concat [if i `mod` 2 == 0 
>                                                           then [((i,j),E) | j <- [0..y-2]]
>                                                           else [((i,j),E) | j <- [1..y-1]] 
>                                                           | i <- [lim+1..x-1]] 
>                             | o == "Vertical"   = concat [if i `mod` 2 == 0 
>                                                           then [((i,j),E) | j <- [0..y-2]]
>                                                           else [((i,j),E) | j <- [1..y-1]] 
>                                                           | i <- [0..lim]] ++
>                                                   concat [if j `mod` 2 == 0 
>                                                           then [((i,j),N) | i <- [lim+1..x-2]]
>                                                           else [((i,j),N) | i <- [lim+1..x-1]]  
>                                                           | j <- [0..y-1]]

> m3 = maze3 10 13 "Horizontal" 3

m3:
+--+--+--+--+--+--+--+--+--+--+
|                 |     |     |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |  |  |  |  |  |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |  |  |  |  |  |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |  |  |  |  |  |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |  |  |  |  |  |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |  |  |  |  |  |
+  +--+--+--+  +  +  +  +  +  +
|              |  |  |  |  |  |
+--+--+--+  +  +  +  +  +  +  +
|              |     |     |  |
+--+--+--+--+--+--+--+--+--+--+

m2:
+--+--+--+--+--+
|     |     |  |
+  +  +  +  +  +
|  |  |  |  |  |
+  +  +  +  +  +
|  |     |     |
+--+--+--+--+--+

m1:
+--+--+--+--+--+
|  |  |  |  |  |
+  +  +--+--+  +
|     |  |  |  |
+--+  +--+--+--+
|        |  |  |
+--+--+  +--+  +
|     |  |     |
+  +  +  +  +--+
|  |        |  |
+--+--+--+--+--+

> m4 = maze3 100 100 "Horizontal" 3

