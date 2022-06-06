{-# LANGUAGE
    ViewPatterns,
    MultiWayIf,
    PatternSynonyms,
    ConstraintKinds,
    NamedFieldPuns,
    DuplicateRecordFields,
    ScopedTypeVariables
#-}

module RbydTree
    ( RbydTree
    , empty
    , emptyChunky
    , singleton
    , singletonChunky
    , append
    , remove
    , create
    , delete
    , lookup
    , (!)
    , (!?)
    , member
    , notMember
    , assocs
    , keys
    , elems
    , foldr
    , foldl
    , foldr'
    , foldl'
    , foldMap
    , foldrWithKey
    , foldrWithKey'
    , foldlWithKey
    , foldlWithKey'
    , foldMapWithKey
    , null
    , size
    , fromList
    , fromListChunky
    , toList
    , compact
    , history
    , height
    , heights
    , weight
    , chunkSize
    , show
    , dump
    , dump2
    , TGrid
    , trender
    , tgrid
    , tungrid
    ) where

import qualified Prelude as P
import Prelude hiding
    ( lookup
    , null
    , flip
    )
import Data.Maybe
import Data.Foldable (foldl', foldr')
import Control.Arrow
import Control.Monad
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

-- miscellany
aligndown :: Integral n => n -> n -> n
aligndown x alignment = x - (x `mod` alignment)

alignup :: Integral n => n -> n -> n
alignup x alignment = aligndown (x + alignment-1) alignment

_uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
_uncurry3 f (a, b, c) = f a b c

_uneither :: Either a a -> a
_uneither (Left  a) = a
_uneither (Right a) = a

padL :: Int -> a -> [a] -> [a]
padL count p l
    | length l >= count = l
    | otherwise         = replicate (count - length l) p ++ l

padR :: Int -> a -> [a] -> [a]
padR count p l
    | length l >= count = l
    | otherwise         = l ++ replicate (count - length l) p

infixl 3 ? 
(?) :: Maybe a -> a -> a
(?) (Just a) _ = a
(?) Nothing  b = b

-- infixl 9 !!?
-- (!!?) :: [a] -> Int -> Maybe a
-- (!!?) [] _ = Nothing
-- (!!?) (a:as) i
--     | i < 0     = Nothing
--     | i == 0    = Just a
--     | otherwise = as !!? (i-1)

data Dir = Lt | Gt
    deriving (Show, Eq)

flipd :: Dir -> Dir
flipd Lt = Gt
flipd Gt = Lt

flipw :: Num k => (k, k) -> k -> k
flipw (lo, hi) w = (lo+hi+1)-w 

data Color = R | B | Y
    deriving (Show, Eq)

data Alt k v = Alt Color Dir k (Node k v)
    deriving Show

-- a number of alt operations
follow :: Ord k => Alt k v -> (k, k) -> Bool
follow (Alt _ Lt w _) (lo, _) = lo < w
follow (Alt _ Gt w _) (_, hi) = hi < w

-- TODO hm, this is complicated
follow' :: (Ord k, Num k) => Dir -> Alt k v -> (k, k) -> Bool
follow' Lt (Alt _ Lt w _) (lo, _ ) = lo < w
follow' Gt (Alt _ Gt w _) (_ , hi) = hi < w
follow' _  (Alt _ Lt w _) (lo, hi) = lo < w && not (w >= lo+hi+1) -- hi /= (lo+hi+1)-w   -- not (hi < (lo+hi+1)-w) -- lo < w && hi /= 0 -- not (hi < (lo+hi+1)-w)
follow' _  (Alt _ Gt w _) (lo, hi) = hi < w && not (w >= lo+hi+1) -- lo /= (lo+hi+1)-w  -- not (lo < (lo+hi+1)-w) -- hi < w && lo /= 0 -- not (lo < (lo+hi+1)-w)

follow2 :: (Ord k, Num k) => Alt k v -> Alt k v -> (k, k) -> Bool
follow2 (Alt _ Lt w1 _) (Alt _ Lt w2 _) (lo, _) = lo < w1+w2
follow2 (Alt _ Gt w1 _) (Alt _ Gt w2 _) (_, hi) = hi < w1+w2
follow2 (Alt _ Lt w1 _) _               (lo, _) = lo < w1
follow2 (Alt _ Gt w1 _) _               (_, hi) = hi < w1

follow2' :: (Ord k, Num k) => Dir -> Alt k v -> Alt k v -> (k, k) -> Bool
follow2' Lt (Alt _ Lt w1 _) (Alt _ Lt w2 _) (lo, _ ) = lo < w1+w2
follow2' Gt (Alt _ Gt w1 _) (Alt _ Gt w2 _) (_,  hi) = hi < w1+w2
follow2' Lt (Alt _ Lt w1 _) _               (lo, _ ) = lo < w1
follow2' Gt (Alt _ Gt w1 _) _               (_,  hi) = hi < w1
follow2' _  (Alt _ Lt w1 _) (Alt _ Lt w2 _) (lo, hi) = lo < w1+w2 && not (w1+w2 >= lo+hi+1)
follow2' _  (Alt _ Gt w1 _) (Alt _ Gt w2 _) (lo, hi) = hi < w1+w2 && not (w1+w2 >= lo+hi+1)
follow2' _  (Alt _ Lt w1 _) _               (lo, hi) = lo < w1    && not (w1    >= lo+hi+1)
follow2' _  (Alt _ Gt w1 _) _               (lo, hi) = hi < w1    && not (w1    >= lo+hi+1)

flipa :: Num k => (k, k) -> Alt k v -> Alt k v
flipa lh (Alt c d w alts) = Alt c (flipd d) (flipw lh w) alts

cull :: Num k => Dir -> k -> (k, k) -> (k, k)
cull Lt w (lo, hi) = (lo-w, hi)
cull Gt w (lo, hi) = (lo, hi-w)

cullf :: Num k => Dir -> k -> (k, k) -> (k, k)
cullf d w lh = cull (flipd d) (flipw lh w) lh

-- change colors
color :: Color -> Alt k v -> Alt k v
color c (Alt _ d k alts) = Alt c d k alts

red :: Alt k v -> Alt k v
red =  color R

black :: Alt k v -> Alt k v
black = color B

yellow :: Alt k v -> Alt k v
yellow = color Y

infixr 5 :::
data Node k v
    = Alt k v ::: Node k v
    | Tag k (Maybe v)
    deriving Show

n_foldr :: (Alt k v -> b -> b) -> b -> Node k v -> b
n_foldr f b (alt ::: alts) = f alt $ n_foldr f b alts
n_foldr _ b (Tag _ _)      = b

_n_foldl' :: (b -> Alt k v -> b) -> b -> Node k v -> b
_n_foldl' f b (alt ::: alts) = P.flip (_n_foldl' f) alts $! f b alt
_n_foldl' _ b (Tag _ _)      = b

n_tag :: (k -> Maybe v -> b) -> Node k v -> b
n_tag f (_ ::: alts) = n_tag f alts
n_tag f (Tag k v)    = f k v

n_map :: (Alt k v -> b) -> Node k v -> [b]
n_map f alts = n_foldr ((:) . f) [] alts

n_height :: Node k v -> Int
n_height alts = n_foldr (const (+1)) 0 alts

-- note how this both reverses the alts and appends the tag
tag :: k -> Maybe v -> [Alt k v] -> Node k v
tag k v alts = foldl' (P.flip (:::)) (Tag k v) alts


data RbydTree k v = RbydTree
    { t_chunkSize :: (Maybe k)
    , t_history :: [(k, Node k v)]
    }

t_weight :: Num k => RbydTree k v -> k
t_weight RbydTree{t_history = (w, _):_} = w
t_weight RbydTree{t_history = []}       = 0

t_head :: RbydTree k v -> Maybe (Node k v)
t_head RbydTree{t_history = (_, alts):_} = Just alts
t_head RbydTree{t_history = []}          = Nothing

t_partition :: Num k => RbydTree k v -> k -> (k, k)
t_partition tree k = (k, (t_weight tree)-1 - k)

t_fix :: Integral k => RbydTree k v -> k -> k -> k
t_fix tree k k' = case t_chunkSize tree of
    Just cz -> (k `aligndown` cz) + (k' `mod` cz)
    Nothing -> k'


-- creation
empty :: RbydTree k v
empty = RbydTree{t_chunkSize = Nothing, t_history = []}

emptyChunky :: k -> RbydTree k v
emptyChunky cz = RbydTree{t_chunkSize = Just cz, t_history = []}

singleton :: Integral k => k -> v -> RbydTree k v
singleton k v = append k v $ empty

singletonChunky :: Integral k => k -> k -> v -> RbydTree k v
singletonChunky cz k v = append k v $ emptyChunky cz

-- append
t_append :: forall k v. Integral k
    => k -> Maybe v -> k -> RbydTree k v
    -> (RbydTree k v, k)
t_append k v delta tree = (tree', delta')
  where
    cz = t_chunkSize tree
    w = t_weight tree

    -- adjust delta if append is outside of current tree
    adj_delta = if
        | isJust v  -> max delta $ ((k+1) `alignup` (cz ? 1)) - w
        | otherwise -> delta

    -- adjust key if we're creating to lookup up a zero-width key
    adj_k = if
        | adj_delta > 0 -> k `aligndown` (cz ? 1)
        | otherwise     -> k

    -- build new node in tree
    tree' = tree{t_history = (w+delta', n') : t_history tree}
    (n', delta') = case t_head tree of
        Nothing   -> (Tag k v, adj_delta)
        Just alts -> append' Nothing (alts, t_partition tree adj_k, [])

    append'
        :: Maybe (Node k v)
        -> (Node k v, (k, k), [Alt k v])
        -> (Node k v, k)
    append' redge (alt ::: alts, lh, nalts)
        = append' redge' (alts', lh', nalts')
      where
        -- find new weights, alts (to chase), and build new alts
        (alts', lh', nalts')
            =   bflip
            <<< id ||| rflop
            <<< ysplit' redge
            -- <<< ysplit yedge <=< left Left
            -- <<< prune
            $ (alts, lh, alt : nalts)
        -- track last red edge
--        yedge' = case nalts' of
--            (Alt B _ _ _):_ -> alts'
--            _               -> yedge
        redge' = case alt of
            Alt R _ _ _ -> Just (alt ::: alts)
            _           -> redge
    append' redge (alts@(Tag k' _), lh@(lo,hi), nalts)
        = (tag k v nalts', clamped_delta)
      where
        fixed_k = t_fix tree (adj_k-lo) k'
        clamped_delta = max adj_delta (-(hi+1))
        nalts' = bsplit' redge (alts, lh, nalts) fixed_k

    -- recolor nodes?
    recolor :: [Alt k v] -> [Alt k v]
    recolor (a1@(Alt _ d1 _ _):a2@(Alt _ d2 _ _):a3@(Alt R d3 _ _):as)
        | (d2 /= d3) && (d1 == d3) = black a2 : red a1 : red a3 : as
        | (d2 /= d3) && (d1 == d2) = black a3 : red a1 : red a2 : as
        | otherwise                = black a1 : red a2 : red a3 : as
    recolor (a1:a2:as) = black a1 : red a2 : as
    recolor (a1:[])    = black a1 : []
    recolor []         = []

    recolor' :: Maybe (Node k v) -> [Alt k v] -> [Alt k v]
    recolor' redge nalts = case nalts of
--        (a1:(Alt _ Lt w2 _):(Alt R Lt w3 _):as)
--                    -> black a1 : Alt Y Lt (w2+w3) (edge Lt redge) : as
--        (a1:(Alt _ Gt w2 _):(Alt R Gt w3 _):as)
--                    -> black a1 : Alt Y Gt (w2+w3) (edge Gt redge) : as
--
--        ((Alt _ Lt w1 _):a2:(Alt R Lt w3 _):as)
--                    -> black a2 : Alt Y Lt (w1+w3) (edge Lt redge) : as
--        ((Alt _ Gt w1 _):a2:(Alt R Gt w3 _):as)
--                    -> black a2 : Alt Y Gt (w1+w3) (edge Gt redge) : as

        a1@(Alt _ d1 w1 _) : a2@(Alt _ d2 w2 _) : (Alt R d3 w3 _) : as
            | d2 == d3 -> black a1 : Alt Y d3 (w2+w3) (edge d3 redge) : as
            | d1 == d3 -> black a2 : Alt Y d3 (w1+w3) (edge d3 redge) : as

        a1:a2:as -> black a1 : red a2 : as
        a1:[]    -> black a1 : []
        []       -> []

    edge :: Dir -> Maybe (Node k v) -> Node k v
    edge d1 (Just (a@(Alt R d2 _ _) ::: ns))
        | d1 == d2  = a ::: ns
        | otherwise = ns
    edge _ _ = error "needed non-red alt"

    -- prune unfollowable edges?
    prune
        :: (Node k v, (k, k), [Alt k v])
        -> Either
            (Node k v, (k, k), [Alt k v])
            (Node k v, (k, k), [Alt k v])
    prune (alts, lh@(lo,hi), nalts) = case nalts of
        (Alt _ _ w1 alts'):a2@(Alt R _ w2 _):as
            | w1+w2 >= lo+hi+1 -> Right (alts', lh, black a2 : as)
        (Alt _ _ w1 alts'):as
            | w1 >= lo+hi+1    -> Left  (alts', lh, as)
        -- do nothing
        as -> Right (alts, lh, as)

    -- split
    ysplit
        :: Node k v
        -> (Node k v, (k, k), [Alt k v])
        -> Either
            (Either
              (Node k v, (k, k), [Alt k v])
              (Node k v, (k, k), [Alt k v]))
            (Node k v, (k, k), [Alt k v])
    ysplit yedge (alts, lh, nalts) = case nalts of
        -- split yellow alts?
        a1@(Alt R d1 w1 alts1) : a2@(Alt R _ w2 _) : as
            -- follow split
            | follow2 a1 a2 lh
                -> Left . Right
                $  (alts1, cullf d1 (w1+w2) lh, black a2 : (recolor (a1f : as)))
            -- force split
            | otherwise
                -> Left . Left
                $  (alts,  cull  d1 (w1+w2) lh, recolor (ay : as))
          where
            a1f = flipa lh $ Alt B d1 (w1+w2) alts
            ay  =            Alt B d1 (w1+w2) yedge
        -- do nothing
        as -> Right (alts, lh, as) 

    -- split
    -- TODO should we process this one deeper? simplify flop checks?
    ysplit'
        :: Maybe (Node k v)
        -> (Node k v, (k, k), [Alt k v])
        -> Either
            (Node k v, (k, k), [Alt k v])
            (Node k v, (k, k), [Alt k v])
    ysplit' redge (alts, lh, nalts) = case nalts of
        -- split yellow alts?
        a1@(Alt Y _ _ _) : as -> Left (alts, lh, recolor' redge (a1 : as))
        -- do nothing
        as -> Right (alts, lh, as)

    -- flop
    rflop
        :: (Node k v, (k, k), [Alt k v])
        -> (Node k v, (k, k), [Alt k v])
    rflop (alts, lh, nalts) = case nalts of
        -- flop red alts?
        a1@(Alt B d1 w1 alts1) : a2@(Alt R d2 w2 _) : as
            | follow a2 lh && follow2 a1 a2 lh
                -> (alts1, cullf d1 (w1+w2) lh, black a2 : red a1f : as)
            | follow a2 lh
                -> (alts,  cull  d1 w1      lh, black a2 : red a1  : as)
            | otherwise
                -> (alts,  cull  d2 w2      lh, a1 : a2 : as)
          where
            a1f = flipa lh $ Alt B d1 (w1+w2) alts
        -- do nothing
        as -> (alts, lh, as) 

    -- flip
    bflip
        :: (Node k v, (k, k), [Alt k v])
        -> (Node k v, (k, k), [Alt k v])
    bflip (alts, lh, nalts) = case nalts of
        -- flip black alts?
        a1@(Alt B d1 w1 alts1) : as
            | follow a1 lh
                -> (alts1, cullf d1 w1 lh, a1f : as)
            | otherwise
                -> (alts,  cull  d1 w1 lh, a1  : as)
          where
            a1f = flipa lh $ Alt B d1 w1 alts
        -- do nothing
        as -> (alts, lh, as)

    -- split leaf node?
    bsplit :: (Node k v, (k, k), [Alt k v]) -> k -> [Alt k v]
    bsplit (alts, (lo,hi), as) k'
        | adj_delta > 0 && k' < adj_k = recolor (Alt B Lt (lo+hi+1) alts : as)
        | adj_delta > 0               = recolor (Alt B Gt (lo+hi+1) alts : as)
        | k' < adj_k                  = recolor (Alt B Lt lo        alts : as)
        | k' > adj_k                  = recolor (Alt B Gt hi        alts : as)
        | otherwise                   = as

    -- split leaf node?
    bsplit' :: Maybe (Node k v) -> (Node k v, (k, k), [Alt k v]) -> k -> [Alt k v]
    bsplit' redge (alts, (lo,hi), as) k'
        | adj_delta > 0 && k' < adj_k = recolor' redge (Alt B Lt (lo+hi+1) alts : as)
        | adj_delta > 0               = recolor' redge (Alt B Gt (lo+hi+1) alts : as)
        | k' < adj_k                  = recolor' redge (Alt B Lt lo        alts : as)
        | k' > adj_k                  = recolor' redge (Alt B Gt hi        alts : as)
        | otherwise                   = as
    
append :: Integral k => k -> v -> RbydTree k v -> RbydTree k v
append k v tree = tree'
  where
    (tree', _) = t_append k (Just v) 0 tree

remove :: Integral k => k -> RbydTree k v -> RbydTree k v
remove k tree = tree'
  where
    (tree', _) = t_append k Nothing 0 tree

-- create/delete for array-like insertions
create :: Integral k => k -> v -> RbydTree k v -> RbydTree k v
create k v tree = case t_chunkSize tree of
    Nothing -> error "attempted to create with no chunkSize"
    Just cz -> tree'
      where
        (tree', _) = t_append k (Just v) cz tree

delete :: forall k v. Integral k => k -> RbydTree k v -> RbydTree k v
delete k tree = case t_chunkSize tree of
    Nothing -> error "attempted to delete with no chunkSize"
    Just cz -> delete' cz tree
      where
        delete' :: k -> RbydTree k v -> RbydTree k v
        delete' 0     tree = tree
        delete' count tree = delete' (count + delta) tree'
          where
            (tree', delta) = t_append (k `aligndown` cz) Nothing (-cz) tree

-- lookup
t_lookup :: forall k v. Integral k => k -> RbydTree k v -> (k, Maybe v, (k, k))
t_lookup k tree = case t_head tree of
    Nothing   -> (k, Nothing, (0, 0))
    Just alts -> lookup' Lt (t_partition tree k) alts
  where
    lookup' :: Dir -> (k, k) -> Node k v -> (k, Maybe v, (k, k))
    lookup' pd lh        (alt@(Alt _ d w alts') ::: alts)
        | follow' pd alt lh = lookup' d (cullf d w lh) alts'
        | otherwise         = lookup' d (cull  d w lh) alts
    lookup' _  lh@(lo,_) (Tag k' v') = (fixed_k, v', lh)
      where
        fixed_k = t_fix tree (k-lo) k'

lookup :: Integral k => k -> RbydTree k v -> Maybe v
lookup k tree
    | k == k'   = v'
    | otherwise = Nothing
  where
    (k', v', _) = t_lookup k tree

(!) :: Integral k => RbydTree k v -> k -> v
(!) tree k = case lookup k tree of
    Just v  -> v
    Nothing -> error $ "lookup failed"

(!?) :: Integral k => RbydTree k v -> k -> Maybe v
(!?) tree k = lookup k tree

member :: Integral k => k -> RbydTree k v -> Bool
member k tree = isJust $ lookup k tree

notMember :: Integral k => k -> RbydTree k v -> Bool
notMember k tree = isNothing $ lookup k tree

-- traversal
assocs :: forall k v. Integral k => RbydTree k v -> [(k, v)]
assocs tree = case t_head tree of
    Nothing   -> []
    Just alts -> assocs' alts 0
  where
    assocs' :: Node k v -> k -> [(k, v)]
    assocs' _    q | q >= t_weight tree = []
    assocs' alts q = case t_lookup q tree of
        (k, Just v,  (_,hi)) -> (k, v) : assocs' alts (q+hi+1)
        (_, Nothing, (_,hi)) ->          assocs' alts (q+hi+1)

keys :: Integral k => RbydTree k v -> [k]
keys tree = map fst $ assocs tree

elems :: Integral k => RbydTree k v -> [v]
elems tree = map snd $ assocs tree

instance Integral k => Foldable (RbydTree k) where
    foldr f b tree = foldr f b $ elems tree

foldrWithKey :: Integral k => (k -> v -> b -> b) -> b -> RbydTree k v -> b
foldrWithKey f b tree = foldr (uncurry f) b $ assocs tree

foldrWithKey' :: Integral k => (k -> v -> b -> b) -> b -> RbydTree k v -> b
foldrWithKey' f b tree = foldr' (uncurry f) b $ assocs tree

foldlWithKey :: Integral k => (b -> k -> v -> b) -> b -> RbydTree k v -> b
foldlWithKey f b tree = foldl (\b (k, v) -> f b k v) b $ assocs tree

foldlWithKey' :: Integral k => (b -> k -> v -> b) -> b -> RbydTree k v -> b
foldlWithKey' f b tree = foldl' (\b (k, v) -> f b k v) b $ assocs tree

foldMapWithKey :: Integral k => Monoid m => (k -> v -> m) -> RbydTree k v -> m
foldMapWithKey f tree = foldMap (uncurry f) $ assocs tree

null :: Integral k => RbydTree k v -> Bool
null tree = P.null $ assocs tree

size :: Integral k => RbydTree k v -> Int
size tree = P.length $ assocs tree

-- list to/from
toList :: Integral k => RbydTree k v -> [(k, v)]
toList tree = assocs tree

fromList :: Integral k => [(k, v)] -> RbydTree k v
fromList = foldl' (P.flip $ uncurry append) empty

fromListChunky :: Integral k => k -> [(k, v)] -> RbydTree k v
fromListChunky cz = foldl' (P.flip $ uncurry append) (emptyChunky cz)

-- compact RbydTree, keeping the effective tree but resetting history
compact :: Integral k => RbydTree k v -> RbydTree k v
compact tree = foldlWithKey' (\t k v -> append k v t) empty' tree
  where empty' = tree{t_history = []}

-- history introspection
history :: RbydTree k v -> [RbydTree k v]
history tree = case t_history tree of
    []   -> [tree]
    _:hs -> tree : history tree{t_history = hs}

height :: RbydTree k v -> Int
height tree = foldr max 0 $ heights tree

heights :: RbydTree k v -> [Int]
heights tree = map (n_height . snd) $ t_history tree

weight :: Num k => RbydTree k v -> k
weight tree = t_weight tree

chunkSize :: RbydTree k v -> Maybe k
chunkSize tree = t_chunkSize tree


-- debugging things
instance (Num k, Show k, Show v) => Show (RbydTree k v) where
    show tree = "RbydTree" ++ dump tree

n_dump :: (Show k, Show v) => Node k v -> String
n_dump n = case n of
    (Tag _ _) -> "(" ++ tag' ++ ")"
    _         -> "(" ++ tag' ++ "; " ++ intercalate "," alts' ++ ")"
  where
    tag' = n_tag t_dump n
    alts' = n_map a_dump n

t_dump :: (Show k, Show v) => k -> Maybe v -> String
t_dump k (Just v) = show k ++ ": " ++ show v
t_dump k Nothing  = show k ++ ": x"

a_dump :: Show k => Alt k v -> String
a_dump (Alt c d w alts) = d' ++ "w" ++ show w ++ c' ++ t' ++ "." ++ o'
  where
    d' = case d of {Lt -> "<"; Gt -> ">"}
    c' = case c of {R -> "r"; B -> "b"; Y -> "y"}
    -- not really sure how to display branches, just showing
    -- the tag at the branch for now
    t' = show $ n_tag (\k _ -> k) alts
    o' = show $ n_height alts

dump :: (Num k, Show k, Show v) => RbydTree k v -> String
dump tree
    =  "{"
    ++ "w" ++ show (t_weight tree)
    ++ "; "
    ++ intercalate ", " (reverse [n_dump n | (_,n) <- t_history tree])
    ++ "}"

dump2 :: forall k v. (Show k, Show v) => RbydTree k v -> String
dump2 tree = unlines [line y | y <- [h-1, h-2..0]]
  where
    h = height tree + 1
    line y = intercalate "  " $ reverse [n_dump2 y n | (_,n) <- t_history tree]

    n_dump2 :: Int -> Node k v -> String
    n_dump2 y n = padR 7 ' ' $ if
        | y == 0             -> tag'
        | y-1 < length alts' -> a_dump (alts' !! (length alts'-1 - (y-1)))
        | otherwise          -> ""
      where
        tag' = n_tag t_dump n
        alts' = n_map id n

type TGrid = Map.Map (Int, Int) String

trender :: Show k => RbydTree k v -> String
trender tree = uncurry tungrid $ tgrid tree

tgrid :: forall k v. Show k => RbydTree k v -> ((Int, Int), TGrid)
tgrid tree = ((w, h), grid)
  where
    b = "\x1b[90mb\x1b[m"
    r = "\x1b[31mr\x1b[m"
    y = "\x1b[33my\x1b[m"

    n_trender :: Node k v -> [String]
    n_trender alts@(_ ::: alts') = cc : n_trender alts'
      where
        cc = case alts of
            (Alt R _ _ _):::(Alt R _ _ _):::_ -> b ++ y
            (Alt Y _ _ _):::_                 -> r ++ r
            (Alt R _ _ _):::_                 -> b ++ r
            _                                 -> b ++ b
    n_trender (Tag k _) = [padL 2 ' ' $ take 2 $ show k]

    cols = [n_trender n | (_, n) <- t_history tree]
    w = length cols
    h = foldr (max . length) 0 cols
    grid = foldl' insertCol Map.empty $ zip [w-1, w-2..] cols
      where
        insertCol :: TGrid -> (Int, [String]) -> TGrid
        insertCol grid (x, col) = foldl' insertAlt grid $ zip3 (repeat x) ys col
          where
            ys = [length col-1, length col-2..]

        insertAlt :: TGrid -> (Int, Int, String) -> TGrid
        insertAlt grid (x, y, alt) = Map.insert (x, y) alt grid

tungrid :: (Int, Int) -> TGrid -> String
tungrid (w, h) grid = unlines [line y | y <- [h-1, h-2..0]]
  where
    line y = concat [Map.lookup (x, y) grid ? "  " | x <- [0..w-1]]

