{-# LANGUAGE BangPatterns, LambdaCase, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports #-}

import Prelude hiding (lookup)
import RbydTree
import Control.Monad
import qualified Data.List as L
import System.Exit
import qualified Data.Foldable as F
import Data.Bits
import qualified Data.Set as S

-- quick deterministic random number generator
-- build on xorshif32
xorshift32 :: Int -> [Int]
xorshift32 range = xorshift32' 42
  where
    xorshift32' x = (x''' `mod` range) : xorshift32' x'''
      where
        x'   = x   `xor` (x   `shift` 13   )
        x''  = x'  `xor` (x'  `shift` (-17))
        x''' = x'' `xor` (x'' `shift` 5    )

-- make unordered list uniqe
uniq :: Ord a => [a] -> [a]
uniq xs = uniq' S.empty xs
  where
    uniq' s (x:xs)
        | S.member x s = uniq' s xs
        | otherwise    = x : uniq' (S.insert x s) xs
    uniq' _ [] = []

-- convenience result type
data Failure k v
    = FailedLookup
        { r_badKey :: !k
        , r_foundValue :: !(Maybe v)
        , r_expectedValue :: !(Maybe v)
        }
    | FailedAssocs
        { r_foundAssocs :: ![(k, v)]
        , r_expectedAssocs :: ![(k, v)]
        }
    deriving Show

data Result k v
    = NoResult
    | Success
        { r_minHeight :: !Int
        , r_maxHeight :: !Int
        , r_worstTree :: !(RbydTree k v)
        }
    | Failure
        { r_minHeight :: !Int
        , r_maxHeight :: !Int
        , r_worstTree :: !(RbydTree k v)
        , r_failure :: Failure k v
        }
    deriving Show

success :: RbydTree k v -> Result k v
success !tree = Success
    { r_minHeight = h
    , r_maxHeight = h
    , r_worstTree = tree
    }
  where h = height tree

failure :: RbydTree k v -> Failure k v -> Result k v
failure !tree failure = Failure
    { r_minHeight = h
    , r_maxHeight = h
    , r_worstTree = tree
    , r_failure = failure
    }
  where h = height tree

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
checkLookup :: (Integral k, Eq v)
    => k -> Maybe v -> RbydTree k v -> Result k v
checkLookup k v tree
    | v' == v   = success tree
    | otherwise = failure tree $ FailedLookup
        { r_badKey = k
        , r_foundValue = v'
        , r_expectedValue = v
        }
  where
    v' = lookup k tree

checkAssocs :: (Integral k, Eq v)
    => [(k, v)] -> RbydTree k v -> Result k v
checkAssocs kvs tree
    | kvs' == kvs = success tree
    | otherwise = failure tree $ FailedAssocs
        { r_foundAssocs = kvs'
        , r_expectedAssocs = kvs
        }
  where
    kvs' = assocs tree

check :: (Integral k, Eq v)
    => [(k, v)] -> RbydTree k v -> Result k v
check kvs tree
    =  foldMap (\(k, v) -> checkLookup k (Just v) tree) kvs
    <> checkAssocs kvs tree

testTree :: [Int] -> Result Int Int
testTree perm = check (L.sort [(x,x) | x <- perm]) tree
  where
    -- build tree
    tree :: RbydTree Int Int
    tree = fromList [(x,x) | x <- perm]

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

    -- test some random trees, don't exit on failure because
    -- we'd rather let the exhaustive testing find a smaller failure case
    let n = 40
    forM_
        [ ("tested in_order size " ++ show n, [0..n-1])
        , ("tested reversed size " ++ show n, [n-1,n-2..0])
        , ("tested random size " ++ show n, take n $ uniq $ xorshift32 (2*n))
        ]
        $ \(s, xs) -> do
            let result = testTree xs
            case result of
                NoResult -> return ()
                r@Success{} -> do
                    putStrLn s
                    putStr $ trender (r_worstTree r)
                f@Failure{} -> do
                    putStrLn $ case r_failure f of
                        f@FailedLookup{}
                            -> "failed lookup k=" ++ show (r_badKey f)
                            ++ ", found " ++ show (r_foundValue f)
                            ++ ", expected " ++ show (r_expectedValue f)
                        f@FailedAssocs{}
                            -> "failed assocs"
                            ++ "\nfound " ++ show (r_foundAssocs f)
                            ++ "\nexpected " ++ show (r_expectedAssocs f)
                    putStr $ trender (r_worstTree f)
                    putStrLn $ show (r_worstTree f)

    -- exhaustively test trees
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
                putStrLn $ case r_failure f of
                    f@FailedLookup{}
                        -> "failed lookup k=" ++ show (r_badKey f)
                        ++ ", found " ++ show (r_foundValue f)
                        ++ ", expected " ++ show (r_expectedValue f)
                    f@FailedAssocs{}
                        -> "failed assocs"
                        ++ "\nfound " ++ show (r_foundAssocs f)
                        ++ "\nexpected " ++ show (r_expectedAssocs f)
                putStr $ trender (r_worstTree f)
                putStrLn $ show (r_worstTree f)
                exitFailure

    putStrLn "done"
