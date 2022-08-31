module FunctionalP
     where

{- 1. groupbyNTail --}
groupbyNTail :: [a] -> Int -> [[a]]
groupbyNTail iL n = grouphelper iL n [] []
    where
        grouphelper [] _ buf lst = lst ++ [buf]
        grouphelper (x:xs) n buf lst 
            | length buf >=n = grouphelper xs n [x] (lst ++ [buf])
            | otherwise = grouphelper xs n (buf ++ [x]) lst


-----------------------------------------------------------

{- 2.  elemAll and stopsAt  --}

{- (a)  elemAll --}
elemAll :: Eq a => [a] -> [a] -> Bool
elemAll xs ys = foldr isInList True xs
    where
        isInList x y = (x `elem` ys) && y  -- returns true if x is in ys and y is true
        

{- (b) stopsAt --}

stopsAt :: Eq a => [a] -> [(b,[a])] -> [b]
stopsAt stops buses = foldr selectBus [] buses
    where
        selectBus (name, ss) xs = -- select bus and add to list if stops in all given stops
            if elemAll stops ss then name : xs  -- if it has all stops, add to list
            else xs         -- else, don't add to list
            
-----------------------------------------------------------

{- 3. isBigger and applyRange --}

--define the Timestamp datatype
data Timestamp =  DATE (Int,Int,Int) |  DATETIME (Int,Int,Int,Int,Int) 
                  deriving (Show, Eq)

{- (a)  isBigger --}
isBigger  ::  Timestamp  ->  Timestamp  ->  Bool
isBigger (DATE (m1,d1,y1)) (DATE (m2,d2,y2))
    | y1 > y2 = True                -- year is bigger
    | y1 == y2 && m1 > m2 = True        -- same year but month is bigger
    | y1 == y2 && m1 == m2 && d1 > d2 = True    -- same year and month but day is bigger
    | otherwise = False
isBigger (DATETIME (m1,d1,y1,h1,mm1)) (DATETIME (m2,d2,y2,h2,mm2))
    | isBigger (DATE (m1,d1,y1)) (DATE (m2,d2,y2)) = True   -- bigger date 
    | y1 == y2 && m1 == m2 && d1 == d2 && h1 > h2 = True    -- equal date, bigger hour
    | y1 == y2 && m1 == m2 && d1 == d2 && h1 == h2 && mm1 > mm2 = True    -- equal date and hour, bigger minutes
    | otherwise = False
isBigger (DATE (m1,d1,y1)) (DATETIME (m2,d2,y2,h2,mm2)) = isBigger (DATE (m1,d1,y1)) (DATE (m2,d2,y2))  -- compare dates
isBigger (DATETIME (m1,d1,y1,h1,mm1)) (DATE (m2,d2,y2)) = isBigger (DATE (m1,d1,y1)) (DATE (m2,d2,y2))  -- compare dates


{- (b) applyRange --}
applyRange  ::  (Timestamp,  Timestamp)  ->  [Timestamp]  ->  [Timestamp]
applyRange (ts1, ts2) xs = foldr addInRange [] xs
    where
        addInRange ts ys = -- add timestamp only if in range
            if isBigger ts ts1 && isBigger ts2 ts then ts : ys  -- if > first and < last, add to list
            else ys        -- otherwise, don't add


-----------------------------------------------------------
{-4 - foldTree, createRTree, fastSearch  --}

--define Tree and RTree data types
data Tree a = LEAF a | NODE a (Tree a) (Tree a)
               deriving (Show,  Eq, Ord)

data RTree a = RLEAF a | RNODE a (a,a) (RTree a) (RTree a)
                    deriving (Show, Eq, Ord)

{- (a) foldTree --}
foldTree :: (t -> t -> t) -> Tree t -> t
foldTree fun (LEAF v) = v   -- don't apply function, just return the value
-- if node, apply function to folded subtrees and apply function to node value and subtree result
foldTree fun (NODE v lt rt) = fun v (fun (foldTree fun lt) (foldTree fun rt))

{- (b) createRTree --}

createRTree :: Ord t => Tree t -> RTree t
createRTree (LEAF v) = RLEAF v    -- a leaf resultf in just a rleaf
-- a node creates a rnode with same value, folds to get min and max in a tuple and recurses in both l and r subtrees
createRTree (NODE v l r) = 
    RNODE v (foldTree min (NODE v l r), foldTree max (NODE v l r)) (createRTree l) (createRTree r)

{- (c) fastSearch --}
fastSearch :: Ord t => RTree t -> t -> [([Char], t)]
fastSearch (RLEAF v) _ = [("leaf", v)]  -- if we visit a leaf, save it
fastSearch (RNODE v (a, b) l r) x = 
    if x >=a && x <= b then  -- if searched value is inside range of tree
        -- recurse search in order left and right, add searches to list
        [("node", v)] ++ (fastSearch l x) ++ (fastSearch r x)
    else 
        [("node", v)]   -- not in subtree, just mark node as visited and return
