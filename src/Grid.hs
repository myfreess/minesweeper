{-# LANGUAGE ViewPatterns #-}

module Grid where

data Content = Blank | Mine | Close Int| Open Int | OMine deriving (Eq, Show)

type Size = Int

type Position = (Int,Int)

type Grid = [[Content]] -- size 9 × 9

type Seed = Int

type Count = Int

type State = (Grid,Count,Seed) -- State

mkGrid :: Size -> Grid
mkGrid n = replicate 9 (replicate 9 Blank)

(.!.) :: Grid -> Position -> Content
g .!. p@(i,j) = g !! (i - 1) !! (j - 1)
-- unsafe

boardSize :: Size
boardSize = 9

board :: Grid
board = mkGrid boardSize

isvalidPos :: Position -> Bool
isvalidPos (i, j) = (i >= 1 && i <= boardSize) && (j >= 1 && j <= boardSize)

formatBoard :: Grid -> String
formatBoard b = topbar ++ (snd $ foldr go ('i', sepLine) b) where
      go :: [Content] -> (Char,String) -> (Char,String)
      go l (c,s) = (pred c, sepLine ++ (c:' ':(foldr walk "|\n" l)) ++ s)
      walk :: Content -> String -> String
      walk Blank s = "| # " ++ s
      walk Mine s = "| # " ++ s
      walk (Open n) s = "| " ++ show n ++ " " ++ s
      walk (Close _) s = "| # " ++ s
      walk OMine s = "| % " ++ s
      sepLine :: String
      sepLine = "  +" ++ replicate (boardSize * 4 - 1) '-' ++ "+\n"
      topbar :: String
      topbar = "    " ++ foldr (\i s -> show i ++ "   " ++ s) "  " [1..boardSize] ++ "\n" 

-- done
-- well defined for size < 10

getPeers :: Position -> [Position]
getPeers (i,j) = filter isvalidPos res where
     res = do { x <- [i,i+1,i-1];
                y <- [j,j+1,j-1];
                return (x,y) }

 
getNearbyMines :: Grid -> Position -> Int -- maybe adjacent
getNearbyMines b p = foldr go 0 $ getPeers p where
    go :: Position -> Int -> Int
    go ((b .!.) -> Mine) n = n + 1
    go _ n = n



landMines :: Seed -> Grid
landMines s = getBoard $ walk ([[]],0,s) (boardSize * boardSize) where
   walk :: State -> Int -> State
   walk sT 0 = sT
   walk sT n = let {(val,seed) = refresh $ getSeed sT;
        content = if val `mod` 6 == 2 then Mine else Blank} in walk (update content seed sT) (n-1)
        
signCount :: Grid -> Grid
signCount b = go id (1,1) b where
    go :: ([Content] -> [Content]) -> Position -> Grid -> Grid
    go f (i,j) [] = []
    go f (i,j) ([]:xss) = f [] : go id (i+1,1) xss
    go f p@(i,j) ((x:xs):xss) = case x of { Blank -> go (f . ((Close (getNearbyMines b p)) :)) (i,j+1) (xs:xss);
     _ -> go (f . (x :)) (i, j+1) (xs:xss) }

 

refresh :: Seed -> (Int,Seed)
refresh seed = let {ans = mod (seed * 16807) 2147483647} in (ans, ans)

update :: Content -> Seed -> State -> State
update elem seed (l@(x:xs),count,_) | count < boardSize = (((elem:x):xs), count+1, seed)
                                    | count == boardSize = ([elem]:l,1,seed)

getSeed :: State -> Seed
getSeed (_,_,s) = s

getBoard :: State -> Grid
getBoard (b,_,_) = b

setQ :: Grid -> Position -> Content -> Grid
setQ b p0@(i,j) content = if isvalidPos p0 then walk id (1,1) b else error "Position not valid" where
    walk f (i,j) [] = undefined
    walk f (i,j) ([]:xss) = f [] : walk id (i+1,1) xss
    walk f p@(i,j) ((x:xs):xss) | p0 == p = (f . (content :)) xs : xss
                                | otherwise = walk (f . (x :)) (i,j+1) (xs:xss)
