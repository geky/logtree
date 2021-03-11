{-# LANGUAGE BangPatterns, LambdaCase #-}
import Prelude hiding (lookup)
import LogTree
import Control.Monad
import Data.List hiding (lookup)

-- convenience result type
data Result
    = NoResult
    | Result !Int !Int
    deriving Show

result :: LogTree k v -> Result
result !tree = Result h h
    where h = height tree

rmin :: Result -> Int
rmin NoResult     = 0
rmin (Result h _) = h

rmax :: Result -> Int
rmax NoResult     = 0
rmax (Result _ h) = h

instance Semigroup Result where
    (<>) (Result mina maxa) (Result minb maxb) = Result (min mina minb) (max maxa maxb)
    (<>) (Result mina maxa) NoResult           = Result mina maxa
    (<>) NoResult           (Result minb maxb) = Result minb maxb
    (<>) NoResult           NoResult           = NoResult

instance Monoid Result where
    mempty = NoResult

-- testing
testTree :: [Int] -> Result
testTree perm = result tree
  where
    tree :: LogTree Int Int
    tree = fromList [(x,x) | x <- perm]

testTrees :: Int -> Result
testTrees n = foldl' (<>) NoResult $ map testTree $ permutations [1..n]

-- entry point
main :: IO ()
main = do
    let tree = fromList [(1, "a"), (2, "b"), (3, "c")]
    print (show tree)
    print (lookup 1 tree)
    print (lookup 2 tree)
    print (lookup 3 tree)
    print (lookup 4 tree)

    forM_ [1..10] $ \n -> do
        let result = testTrees n
        putStrLn
            $  "tested all trees of size " ++ show n
            ++ ", height min " ++ show (rmin result)
            ++ " max " ++ show (rmax result)

    putStrLn "done"
