module Data.Either.Utils
    ( spanLeft
    , leftMergeMap
    , leftMerge
    , ltrMerge
    )
where

spanLeft :: [Either a b] -> ([a], [Either a b])
spanLeft (Left a : xs) = (a : as, xs')
  where
    (as, xs') = spanLeft xs
spanLeft xs = ([], xs)

leftMergeMap :: ([a] -> c) -> (b -> c) -> [Either a b] -> [c]
leftMergeMap f g = go
  where
    go [] = []
    go (Right b : xs) = g b : go xs
    go xs = f ys : go xs'
      where
        (ys, xs') = spanLeft xs

leftMerge :: ([a] -> a) -> [Either a b] -> [Either a b]
leftMerge f = leftMergeMap (Left . f) Right

ltrMerge :: ([a] -> b) -> [Either a b] -> [b]
ltrMerge f = leftMergeMap f id
