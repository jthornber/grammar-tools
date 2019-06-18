module CommonPrefix (
     commonPrefix
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type PrefixMap a = Map [a] [[a]]

commonPrefixLen :: (Eq a) => [[a]] -> Int
commonPrefixLen xs =
    if any null xs
        then 0
        else if all (== h) ts
            then 1 + commonPrefixLen (map tail xs)
            else 0
    where
        heads' = map head xs
        h = head heads'
        ts = tail heads'

-- Takes a list of non-null lists
commonPrefix :: (Eq a, Ord a) => [[a]] -> [([a], [[a]])]
commonPrefix = M.toList . extendPrefix . foldr doOne M.empty
    where
        doOne :: (Ord a) => [a] -> PrefixMap a -> PrefixMap a
        doOne (x:xs) pm = case M.lookup [x] pm of
            Just tails -> M.insert [x] (xs:tails) pm
            Nothing -> M.insert [x] [xs] pm
        doOne [] pm = M.insert [] [[]] pm

        extendPrefix :: (Ord a) => PrefixMap a -> PrefixMap a
        extendPrefix = M.fromList . map extend . M.toList

        extend :: (Eq a) => ([a], [[a]]) -> ([a], [[a]])
        extend p@(prefix, tails) =
            if len > 0
                then (prefix ++ (take len $ head tails), map (drop len) tails)
                else p
            where
                len = commonPrefixLen tails

{-
example = [
    "abcdef",
    "acbdef",
    "abcdglk",
    "libio",
    "libvirt",
    "dfuio",
    "skdjfls",
    "",
    "dldkjf",
    "dfuiok"]

main = putStrLn . show . commonPrefix $ example
-}
