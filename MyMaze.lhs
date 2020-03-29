> module MyMaze (
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size

> )
> where

> import Geography

***************************************
*            Question 5               *
***************************************

> data Maze = AMaze Size [Place] [Place] [Place] [Place]

We build the maze by building each list of places that have walls at one of the 4 directions.
The list of places that have a wall at N, north, consists of the places with a wall at N from
the given list walls plus the boundary walls (i.e. the list north') and plus the reflected 
places that have a wall at S (i.e. map (move S) south')
Similarly we can define the list of places that have a wall at S/E/W.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = AMaze (x,y) north south east west
>    where north' = map fst (filter (\x -> snd x == N) (walls)) ++ 
>                   [(i,y-1) | i <- [0..(x-1)]]
>          south' = map fst (filter (\x -> snd x == S) (walls)) ++ 
>                   [(i,0) | i <- [0..(x-1)]]
>          east'  = map fst (filter (\x -> snd x == E) (walls)) ++ 
>                   [(x-1,i) | i <- [0..(y-1)]]
>          west'  = map fst (filter (\x -> snd x == W) (walls)) ++ 
>                   [(0,i) | i <- [0..(y-1)]]
>          north  = north' ++ map (move S) south' 
>          south  = south' ++ map (move N) north'
>          east   = east' ++ map (move W) west'   
>          west   = west' ++ map (move E) east'   

A maze has a wall at d in the place pl iff pl is an element of the list of places that have a 
wall at d.

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ north south east west) pl d | d == N = pl `elem` north
>                                              | d == S = pl `elem` south
>                                              | d == E = pl `elem` east
>                                              | d == W = pl `elem` west

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size

Old representation (Maze.lhs) time and memory:

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,
E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,
W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,
S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,
W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,
E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.09 secs, 4,416,152 bytes)

New representation (MyMaze.lhs) time and memory:

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,
E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,
W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,
S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,
W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,
E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.05 secs, 4,553,384 bytes)
