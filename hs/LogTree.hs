{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module LogTree where

import Prelude hiding (lookup, map, head, minimum, maximum)
import qualified Prelude as P
import Data.Maybe
import qualified Data.List as L


data Dir = Lt | Gte
    deriving Show

data Color = R | B | Y
    deriving Show

data Alt k v = Alt Dir Color Color k (Node k v)
    deriving Show

data Node k v = Node k v [Alt k v]
    deriving Show

-- we don't really need to keep the full array, but
-- we keep it around to track the history 
data LogTree k v = LogTree [Node k v]
    deriving Show

-- append
empty :: LogTree k v
empty = LogTree []

singleton :: Ord k => k -> v -> LogTree k v
singleton k v = append k v empty

-- Note, this is currently unbalanced
append :: Ord k => k -> v -> LogTree k v -> LogTree k v
append k v (LogTree [])     = LogTree [(Node k v [])]
append k v (LogTree (n:ns)) = LogTree ((mknode k v n []):n:ns)
  where
    mknode :: Ord k => k -> v -> Node k v -> [Alt k v] -> Node k v
    mknode k v n@(Node k' v' alts) nalts = case alts of
        [] | k' < k    -> Node k v $ reverse ((Alt Lt  B B k  n):nalts)
           | k' > k    -> Node k v $ reverse ((Alt Gte B B k' n):nalts)
           | otherwise -> Node k v $ reverse nalts

        ((Alt Lt  c0 c1 k' n'):alts')
            | k < k'    -> mknode k v n' ((Alt Gte c1 c0 k' (Node k' v' alts')):nalts)
            | otherwise -> mknode k v (Node k' v' alts') ((Alt Lt c0 c1 k' n'):nalts)

        ((Alt Gte c0 c1 k' n'):alts')
            | k >= k'   -> mknode k v n' ((Alt Lt c1 c0 k' (Node k' v' alts')):nalts)
            | otherwise -> mknode k v (Node k' v' alts') ((Alt Gte c0 c1 k' n'):nalts)
      where
        ncons :: Ord k => Alt k v -> [Alt k v] -> [Alt k v]
        ncons = undefined
        

-- lookup
lookup :: Ord k => k -> LogTree k v -> Maybe v
lookup _ (LogTree [])    = Nothing
lookup k (LogTree (n:_)) = lookup' k n
  where
    lookup' :: Ord k => k -> Node k v -> Maybe v
    lookup' k (Node k' v alts) = case alts of
        [] | k == k'                     -> Just v
           | otherwise                   -> Nothing
        ((Alt Lt  _ _ k' n):_) | k <  k' -> lookup' k n
        ((Alt Gte _ _ k' n):_) | k >= k' -> lookup' k n
        (_:alts)                         -> lookup' k (Node k' v alts)

(!) :: (Ord k, Show k) => LogTree k v -> k -> v
(!) tree k = case lookup k tree of
    Just v  -> v
    Nothing -> error $ "lookup failed " ++ show k

(!?) :: Ord k => LogTree k v -> k -> Maybe v
(!?) tree k = lookup k tree

member :: Ord k => k -> LogTree k v -> Bool
member k tree = isJust $ lookup k tree

notMember :: Ord k => k -> LogTree k v -> Bool
notMember k tree = isNothing $ lookup k tree

-- traversal
minimum :: LogTree k v -> Maybe (k, v)
minimum (LogTree [])    = Nothing
minimum (LogTree (n:_)) = Just $ minimum' n
  where
    minimum' :: Node k v -> (k, v)
    minimum' (Node k v alts) = case alts of
        []                    -> (k, v)
        ((Alt Lt  _ _ _ n):_) -> minimum' n
        (_:alts)              -> minimum' (Node k v alts)

maximum :: LogTree k v -> Maybe (k, v)
maximum (LogTree [])    = Nothing
maximum (LogTree (n:_)) = Just $ maximum' n
  where
    maximum' :: Node k v -> (k, v)
    maximum' (Node k v alts) = case alts of
        []                    -> (k, v)
        ((Alt Gte _ _ _ n):_) -> maximum' n
        (_:alts)              -> maximum' (Node k v alts)

assocs :: Ord k => LogTree k v -> [(k, v)]
assocs (LogTree [])         = []
assocs (LogTree (n:_)) = assocs' Nothing n
  where
    assocs' :: Ord k => Maybe k -> Node k v -> [(k, v)]
    assocs' k n = case lookupWithSucc k Nothing n of
        Nothing              -> []
        Just (k, v, Nothing) -> [(k, v)]
        Just (k, v, Just k') -> (k, v) : assocs' (Just k') n

    lookupWithSucc :: Ord k => Maybe k -> Maybe k -> Node k v -> Maybe (k, v, Maybe k)
    lookupWithSucc k hi (Node k' v alts) = case alts of
        []
            | all (== k') k -> Just (k', v, hi)
            | otherwise     -> Nothing
        ((Alt Lt  _ _ k' n):alts)
            | all (< k') k  -> lookupWithSucc k (Just $ foldr min k' hi) n
            | otherwise     -> lookupWithSucc k hi (Node k' v alts)
        ((Alt Gte _ _ k' n):alts)
            | any (>= k') k -> lookupWithSucc k hi n
            | otherwise     -> lookupWithSucc k (Just $ foldr min k' hi) (Node k' v alts)
    

keys :: Ord k => LogTree k v -> [k]
keys tree = P.map fst $ assocs tree

elems :: Ord k => LogTree k v -> [v]
elems tree = P.map snd $ assocs tree

instance Ord k => Foldable (LogTree k) where
    foldr f b tree = foldr f b $ elems tree

foldrWithKey :: Ord k => (k -> v -> b -> b) -> b -> LogTree k v -> b
foldrWithKey f b tree = foldr (uncurry f) b $ assocs tree

-- TODO 
span :: Ord k => k -> k -> LogTree k v -> [v]
span k1 k2 tree = P.map snd $ spanWithKey k1 k2 tree

spanWithKey :: Ord k => k -> k -> LogTree k v -> [(k, v)]
spanWithKey = undefined

null :: Ord k => LogTree k v -> Bool
null tree = P.null $ assocs tree

size :: Ord k => LogTree k v -> Int
size tree = length $ assocs tree

-- list to/from
toList :: Ord k => LogTree k v -> [(k, v)]
toList tree = assocs tree

fromList :: Ord k => [(k, v)] -> LogTree k v
fromList = L.foldl' (flip $ uncurry append) empty

-- history introspection
history :: LogTree k v -> [LogTree k v]
history (LogTree [])     = []
history (LogTree (n:ns)) = LogTree (n:ns) : history (LogTree ns)

heights :: LogTree k v -> [Int]
heights (LogTree ns) = P.map (\(Node _ _ ns) -> length ns) ns

height :: LogTree k v -> Int
height tree = foldr max 0 $ heights tree






------foldWithKey :: (k -> v -> b -> b) -> b -> LogTree k v -> b
------foldWithKey f b tree = undefined
------  where
------    lookupWithSucc :: Ord k => k -> [Tag k v] -> Maybe k -> (Maybe v, Maybe k)
------    lookupWithSucc _ []                     _            = (Nothing, Nothing)
------    lookupWithSucc k ((Tag k' v):_)         hi | k == k' = (Just v, hi)
------    lookupWithSucc k ((AltLt _ _ k' ts):_)  hi | k < k'  = lookupWithSucc k ts (Just $ foldr min k' hi)
------    lookupWithSucc k ((AltGte _ _ k' ts):_) hi | k >= k' = lookupWithSucc k ts hi
------    lookupWithSucc k ((AltGte _ _ k' _):ts) hi           = lookupWithSucc k ts (Just $ foldr min k' hi)
------    lookupWithSucc k (_:ts)                 hi           = lookupWithSucc k ts hi
------
------    foldWithKey' 
------     
----
----
----
------ append
----empty :: LogTree k v
----empty = LogTree []
----
----append :: Ord k => k -> v -> LogTree k v -> LogTree k v
----append k v (LogTree rest) = LogTree ((k, v):rest)
----
------ lookup
----lookup :: Ord k => k -> LogTree k v -> Maybe v
----lookup _ (LogTree []) = Nothing
----lookup k (LogTree ((k', v):rest))
----    | k == k'   = Just v
----    | otherwise = lookup k (LogTree rest)
----
----(!) :: (Ord k, Show k) => LogTree k v -> k -> v
----(!) tree k = case lookup k tree of
----    Just v  -> v
----    Nothing -> error $ "lookup failed " ++ show k
----
----member :: Ord k => k -> LogTree k v -> Bool
----member k tree = isJust $ lookup k tree
----
----notMember :: Ord k => k -> LogTree k v -> Bool
----notMember k tree = not $ member k tree
----
------ traversal
----map :: (a -> b) -> LogTree k a -> LogTree k b
----map f (LogTree rest) = LogTree ([(k, f v) | (k, v) <- rest])
----
----assocs :: LogTree k v -> [(k, v)]
----assocs (LogTree rest) = rest
----
----keys :: LogTree k v -> [k]
----keys (LogTree rest) = [k | (k, _) <- rest]
----
----elems :: LogTree k v -> [v]
----elems (LogTree rest) = [v | (_, v) <- rest]
----
----fold :: (a -> b -> b) -> b -> LogTree k a -> b
----fold f b tree = foldr f b $ elems tree
----
----foldWithKey :: (k -> a -> b -> b) -> b -> LogTree k a -> b
----foldWithKey f b tree = foldr (uncurry f) b $ assocs tree
----
----null :: LogTree k v -> Bool
----null = Prelude.null . assocs
----
----size :: LogTree k v -> Int
----size = length . assocs
----
------ list to/from
----toList :: LogTree k v -> [(k, v)]
----toList = assocs
----
----fromList :: Ord k => [(k, v)] -> LogTree k v
----fromList = foldl' (flip $ uncurry append) empty
----
------ satisfied classes
----instance Functor (LogTree k) where
----    fmap = map
----
----instance Foldable (LogTree k) where
----    foldr = fold
----
----instance Traversable (LogTree k) where
----    traverse f (LogTree rest) = LogTree <$> (liftA . zip) ks (traverse f vs)
----      where
----        (ks, vs) = unzip rest
----
