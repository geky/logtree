{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports #-}

import Prelude hiding (lookup)
import RbydTree
import Control.Monad
import qualified Data.List as L
import System.Exit
import qualified Data.Foldable as F

-- convenience result type
data Result k v
    = NoResult
    | Success
        { r_minHeight :: !Int
        , r_maxHeight :: !Int
        , r_worstTree :: !(RbydTree k v)
        }
    | Failure
        { r_badTree :: !(RbydTree k v)
        , r_badKey :: !k
        , r_badValue :: !(Maybe v)
        , r_expectedValue :: !(Maybe v)
        }
    deriving Show

success :: RbydTree k v -> Result k v
success !tree = Success
    { r_minHeight = h
    , r_maxHeight = h
    , r_worstTree = tree
    }
    where h = height tree

failure :: RbydTree k v -> k -> Maybe v -> Maybe v -> Result k v
failure !tree badK badV expectedV = Failure
    { r_badTree = tree
    , r_badKey = badK
    , r_badValue = badV
    , r_expectedValue = expectedV
    }

instance Semigroup (Result k v) where
    (<>) r            NoResult     = r
    (<>) NoResult     r            = r
    (<>) f@Failure{}  _            = f
    (<>) _            f@Failure{}  = f
    (<>) r1@Success{} r2@Success{} = Success
        { r_minHeight = min (r_minHeight r1) (r_minHeight r2)
        , r_maxHeight = max (r_maxHeight r1) (r_maxHeight r2)
        , r_worstTree = if
            | r_maxHeight r1 >= r_maxHeight r2 -> r_worstTree r1
            | otherwise                        -> r_worstTree r2
        }

instance Monoid (Result k v) where
    mempty = NoResult

-- testing
testTree :: [Int] -> Result Int Int
testTree perm = foldMap check perm
  where
    -- build tree
    tree :: RbydTree Int Int
    tree = fromList [(x,x) | x <- perm]

    -- test all lookups
    check :: Int -> Result Int Int
    check k
        | v == Just k = success tree
        | otherwise   = failure tree k v (Just k)
      where
        v = lookup k tree

testTrees :: Int -> Result Int Int
testTrees n = F.foldMap' testTree $ L.permutations [1..n]


-- entry point
main :: IO ()
main = do
    let tree = fromList [(1, "a"), (2, "b"), (3, "c"), (4, "d"), (5, "e"), (6, "f")]
    print (tree)
    print (lookup 1 tree)
    print (lookup 2 tree)
    print (lookup 3 tree)
    print (lookup 4 tree)
    print (lookup 5 tree)
    print (lookup 6 tree)
    print (assocs tree)
    putStr (trender tree)

    forM_ [1..] $ \n -> do
        let result = testTrees n
        case result of
            NoResult -> do
                putStrLn $ "tested all trees of size " ++ show n
                    ++ ", height min " ++ show 0
                    ++ " max " ++ show 0
            r@Success{} -> do
                putStrLn $ "tested all trees of size " ++ show n
                    ++ ", height min " ++ show (r_minHeight r)
                    ++ " max " ++ show (r_maxHeight r)
                putStr $ trender (r_worstTree r)
            f@Failure{} -> do
                putStrLn $ "failed for k=" ++ show (r_badKey f)
                    ++ ", expected " ++ show (r_expectedValue f)
                    ++ ", found " ++ show (r_badValue f)
                putStr $ trender (r_badTree f)
                putStrLn $ show (r_badTree f)
                exitFailure

    putStrLn "done"
