module Internal.Misc where

import Data.List (sort)
import Internal.Bytes (U8(..))
import qualified Data.Map as Map (Map, keys)
import qualified Data.ByteString as B (ByteString, cons, length)

-- Provide some various helpers that don't really belong in a specific module

getNextKey :: (k -> U8) -> Map.Map k a -> U8
getNextKey f = (+) 1 . maximum' . map f . Map.keys

maximum' :: [U8] -> U8
maximum' [] = U8 0
maximum' xs = maximum xs

reorder :: [a] -> [Int] -> Maybe [a]
reorder elems idxs
  | null elems && null idxs = Just []
  | uniqueSpan idxs && (length idxs == length elems)
    = Just . foldr (reorder' elems . flip (-) 1) [] $ idxs
  | otherwise = Nothing
  where
    reorder' :: [a] -> Int -> [a] -> [a]
    reorder' elems idx = (:) (elems !! idx)

uniqueSpan :: [Int] -> Bool
uniqueSpan idxs = (==) [1..l] . sort $ idxs
  where
    l = length idxs

maybeHead :: [a] -> Maybe a
maybeHead xs
  | null xs = Nothing
  | otherwise = Just $ head xs

-- here we tag the bytestring with its size by appending
-- the size to the front
tagSize :: B.ByteString -> B.ByteString
tagSize bytes = B.cons len bytes
  where
    len = toEnum . B.length $ bytes
