{-# LANGUAGE
    ViewPatterns,
    MultiWayIf,
    PatternSynonyms,
    ConstraintKinds,
    NamedFieldPuns,
    DuplicateRecordFields,
    ScopedTypeVariables
#-}
-- TODO remove me
{-# OPTIONS_GHC -Wno-unused-imports -Wno-redundant-constraints #-}

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
import Control.Applicative ((<|>))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Debug.Trace

-- miscellany
aligndown :: Integral n => n -> n -> n
aligndown x alignment = x - (x `mod` alignment)

alignup :: Integral n => n -> n -> n
alignup x alignment = aligndown (x + alignment-1) alignment

infixl 3 ? 
(?) :: Maybe a -> a -> a
(?) (Just a) _ = a
(?) Nothing  b = b

data Dir = Lt | Gt
    deriving (Show, Eq)

flip' :: Dir -> Dir
flip' Lt = Gt
flip' Gt = Lt

data Color = R | B
    deriving (Show, Eq)

data Alt k v = Alt Color Dir k (Node k v)
    deriving Show

-- a number of alt operations
follow :: Ord k => Alt k v -> (k, k) -> Bool
follow (Alt _ Lt w _) (lo, _) = lo < w
follow (Alt _ Gt w _) (_, hi) = hi < w

follow2 :: (Ord k, Num k) => Alt k v -> Alt k v -> (k, k) -> Bool
follow2 (Alt _ Lt w1 _) (Alt _ Lt w2 _) (lo, _ ) = lo < w1+w2
follow2 (Alt _ Gt w1 _) (Alt _ Gt w2 _) (_ , hi) = hi < w1+w2
follow2 _               _               (_ , _ ) = False

cull :: (Ord k, Num k) => Alt k v -> (k, k) -> (k, k)
cull (Alt _ Lt w _) (lo, hi)
    | lo < w    = (lo,               hi-((lo+hi+1)-w))
    | otherwise = (lo-w,             hi)
cull (Alt _ Gt w _) (lo, hi)
    | hi < w    = (lo-((lo+hi+1)-w), hi)
    | otherwise = (lo,               hi-w)

-- change colors
color :: Color -> Alt k v -> Alt k v
color c (Alt _ d k ns) = Alt c d k ns

red :: Alt k v -> Alt k v
red =  color R

black :: Alt k v -> Alt k v
black = color B

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

biweight :: Num k => RbydTree k v -> k -> (k, k)
biweight tree k = (k, (t_weight tree)-1 - k)

fix :: Integral k => RbydTree k v -> k -> k -> k
fix tree k k' = case t_chunkSize tree of
    Just cz -> (k `aligndown` cz) + (k' `mod` cz)
    Nothing -> k'


-- creation
empty :: RbydTree k v
empty = RbydTree{t_chunkSize = Nothing, t_history = []}

emptyChunky :: k -> RbydTree k v
emptyChunky cz = RbydTree{t_chunkSize = Just cz, t_history = []}

singleton :: (Integral k, Show k, Show v) => k -> v -> RbydTree k v
singleton k v = append k v $ empty

singletonChunky :: (Integral k, Show k, Show v) => k -> k -> v -> RbydTree k v
singletonChunky cz k v = append k v $ emptyChunky cz

-- append
t_append :: forall k v. (Integral k, Show k, Show v)
    => k -> Maybe v -> k -> RbydTree k v
    -> (RbydTree k v, k)
t_append k v delta tree = {-trace "hi"-} (tree', delta')
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
        Just alts -> append' alts (biweight tree adj_k) alts []

    append' :: Node k v
        -> (k, k) -> Node k v -> [Alt k v]
        -> (Node k v, k)
    append' yin lh (alt ::: alts) nalts
        = append' yin' lh' alts' nalts'
      where
        -- find new weights, alts (to chase), and build alts
        (lh', alts', nalts') = prune yin lh alts (alt:nalts)
        -- track incoming edge
        yin' = case nalts' of
            (Alt B _ _ _):_ -> alts'
            _               -> yin
    append' _ lh@(lo,hi) alts@(Tag k' _) nalts
        = (tag k v nalts', clamped_delta)
      where
        fixed_k = fix tree (adj_k-lo) k'
        clamped_delta = max adj_delta (-(hi+1))
        nalts' = bsplit fixed_k lh alts nalts

    -- recolor nodes?
    recolor :: [Alt k v] -> [Alt k v]
    recolor (a1@(Alt _ d1 _ _):a2@(Alt _ d2 _ _):a3@(Alt R d3 _ _):as)
        | (d2 /= d3) && (d1 == d3) = black a2 : red a1 : red a3 : as
        | (d2 /= d3) && (d1 == d2) = black a3 : red a1 : red a2 : as
        | otherwise                = black a1 : red a2 : red a3 : as
    recolor (a1:a2:as) = black a1 : red a2 : as
    recolor [a1]       = [black a1]    
    recolor []         = []

    -- prune unfollowable edges?
    prune :: Node k v
        -> (k, k) -> Node k v -> [Alt k v]
        -> ((k, k), Node k v, [Alt k v])
    prune yin lh@(lo,hi) alts nalts = case nalts of
        (Alt _ _ w1 alts'):a2@(Alt R _ w2 _):as | w1+w2 >= lo+hi+1
            -> rflop lh alts' (black a2 : as)
        (Alt _ _ w1 alts'):as | w1 >= lo+hi+1
            -> done  lh alts' as

        -- continue
        as  -> ysplit yin lh alts as

    -- split yellow alts?
    ysplit :: Node k v
        -> (k, k) -> Node k v -> [Alt k v]
        -> ((k, k), Node k v, [Alt k v])
    ysplit yin lh@(lo,hi) alts nalts = case nalts of
        a1@(Alt R d1 w1 alts'):a2@(Alt R _ w2 _):as
            -- follow split
            | follow2 a1 a2 lh
                -> let a1' = Alt B (flip' d1) ((lo+hi+1)-(w1+w2)) alts
                   in rflop (cull a1' lh) alts' (black a2 : (recolor (a1':as)))
            -- force split
            | otherwise
                -> let a' = Alt B d1 (w1+w2) yin
                   in done  (cull a'  lh) alts  (recolor (a':as))

        -- continue
        as -> rflop lh alts as

    -- flop red alts?
    rflop :: (k, k) -> Node k v -> [Alt k v]
        -> ((k, k), Node k v, [Alt k v])
    rflop lh@(lo,hi) alts nalts = case nalts of
        a1@(Alt B d1 w1 alts'):a2@(Alt R _ w2 _):as
            | follow a2 lh && follow2 a1 a2 lh
                -> let a1' = Alt R (flip' d1) ((lo+hi+1)-(w1+w2)) alts
                   in bflip (cull a1' lh) alts' (black a2 : red a1' : as)
            | follow a2 lh
                ->    bflip (cull a1  lh) alts  (black a2 : red a1  : as)
            | otherwise
                ->    bflip (cull a2  lh) alts  (a1:a2:as)

        -- continue
        as -> bflip lh alts as

    -- flip/follow black alts?
    bflip :: (k, k) -> Node k v -> [Alt k v]
        -> ((k, k), Node k v, [Alt k v])
    bflip lh@(lo,hi) alts nalts = case nalts of
        a1@(Alt B d1 w1 alts'):as
            -- | follow_ a1 lh /= f1 = error $ "what " ++ show (a1:as)
            | follow a1 lh
                -> let a1' = Alt B (flip' d1) ((lo+hi+1)-w1) alts
                   in done (cull a1' lh) alts' (a1':as)
            | otherwise
                ->    done (cull a1  lh) alts  (a1 :as)

        -- continue
        as -> done lh alts as

    -- base case for making the code a bit cleaner
    done :: (k, k) -> Node k v -> [Alt k v]
        -> ((k, k), Node k v, [Alt k v])
    done lh alts as = (lh, alts, as)

    -- split leaf node?
    bsplit :: k -> (k, k) -> Node k v -> [Alt k v] -> [Alt k v]
    bsplit k' (lo,hi) alts as
        | adj_delta > 0 && k' < adj_k = recolor (Alt B Lt (lo+hi+1) alts : as)
        | adj_delta > 0               = recolor (Alt B Gt (lo+hi+1) alts : as)
        | k' < adj_k                  = recolor (Alt B Lt lo        alts : as)
        | k' > adj_k                  = recolor (Alt B Gt hi        alts : as)
        | otherwise                   = as
    
append :: (Integral k, Show k, Show v) => k -> v -> RbydTree k v -> RbydTree k v
append k v tree = tree'
  where
    (tree', _) = t_append k (Just v) 0 tree

remove :: (Integral k, Show k, Show v) => k -> RbydTree k v -> RbydTree k v
remove k tree = tree'
  where
    (tree', _) = t_append k Nothing 0 tree

-- create/delete for array-like insertions
create :: (Integral k, Show k, Show v) => k -> v -> RbydTree k v -> RbydTree k v
create k v tree = case t_chunkSize tree of
    Nothing -> error "attempted to create with no chunkSize"
    Just cz -> tree'
      where
        (tree', _) = t_append k (Just v) cz tree

delete :: forall k v. (Integral k, Show k, Show v) => k -> RbydTree k v -> RbydTree k v
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
    Just alts -> lookup' (biweight tree k) alts
  where
    lookup' :: (k, k) -> Node k v -> (k, Maybe v, (k, k))
    lookup' lh        (alt@(Alt _ _ _ alts') ::: alts)
        | follow alt lh = lookup' (cull alt lh) alts'
        | otherwise     = lookup' (cull alt lh) alts
    lookup' lh@(lo,_) (Tag k' v') = (fixed_k, v', lh)
      where
        fixed_k = fix tree (k-lo) k'

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

fromList :: (Integral k, Show k, Show v) => [(k, v)] -> RbydTree k v
fromList = foldl' (P.flip $ uncurry append) empty

fromListChunky :: (Integral k, Show k, Show v) => k -> [(k, v)] -> RbydTree k v
fromListChunky cz = foldl' (P.flip $ uncurry append) (emptyChunky cz)

-- compact RbydTree, keeping the effective tree but resetting history
compact :: (Integral k, Show k, Show v) => RbydTree k v -> RbydTree k v
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
dump :: forall k v. (Show k, Show v, Show v) => RbydTree k v -> String
dump tree
    =  "{"
    ++ intercalate ", " (reverse $ map (n_dump . snd) $ t_history tree)
    ++ "}"
  where
    n_dump :: Node k v -> String
    n_dump n = case n of
        (Tag _ _) -> "(" ++ tag' ++ ")"
        _         -> "(" ++ tag' ++ "; " ++ intercalate "," alts' ++ ")"
      where
        tag' = n_tag t_dump n
        alts' = n_map a_dump n

    t_dump :: k -> Maybe v -> String
    t_dump k (Just v) = show k ++ ": " ++ show v
    t_dump k Nothing  = show k ++ ": x"

    a_dump :: Alt k v -> String
    a_dump (Alt c d w alts) = d' ++ "w" ++ show w ++ c' ++ t'
      where
        d' = case d of {Lt -> "<"; Gt -> ">"}
        c' = case c of {R -> "r"; B -> "b"}
        -- not really sure how to display branches, just showing
        -- the tag at the branch for now
        t' = show $ n_tag (\k _ -> k) alts

instance (Show k, Show v) => Show (RbydTree k v) where
    show tree = "RbydTree" ++ dump tree

type TGrid = Map.Map (Int, Int) String

trender :: Show k => RbydTree k v -> String
trender tree = uncurry tungrid $ tgrid tree

padL :: Int -> a -> [a] -> [a]
padL count p l
    | length l >= count = l
    | otherwise         = replicate (count - length l) p ++ l

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
            (Alt R _ _ _):::_                 -> b ++ r
            _                                 -> b ++ b
    n_trender (Tag k _) = [padL 2 ' ' $ take 2 $ show k]

    cols = map (n_trender . snd) $ t_history tree
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

