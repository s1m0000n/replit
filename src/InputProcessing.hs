module InputProcessing where

import Config ( shellPrefix, builtInCommandPrefix )
import Data.List (maximumBy)
import Data.Function (on)


trimPrefix :: Eq a => [a] -> [a] -> (Bool, [a])
trimPrefix (p:ps) (x:xs) 
  | p == x = trimPrefix ps xs
  | otherwise = (False, xs)
trimPrefix [] xs = (True, xs)

specialPrefixes :: [String]
specialPrefixes = [shellPrefix, builtInCommandPrefix]

findPrefix :: [Char] -> Maybe (String, String)
findPrefix xs
  | null matchedPrefixesMap = Nothing
  | body == "" = Nothing
  | otherwise = Just (prefix, body)
  where
      matchedPrefixesMap
        = map (\ (_, k, v) -> (k, v))
            $ filter (\ (b, k, v) -> b)
                $ zipWith (\ k (b, v) -> (b, k, v)) specialPrefixes
                    $ map (`trimPrefix` xs) specialPrefixes
      (prefix, body)
        = maximumBy
            (compare `on` (\ (k, v) -> length v)) matchedPrefixesMap

tokenize :: String -> [String]
tokenize text = case findPrefix firstToken of
    Nothing -> tokens
    Just (tokenPrefix, tokenTail) -> tokenPrefix:tokenTail:otherTokens
 where 
  tokens = words text
  firstToken = head tokens
  otherTokens = tail tokens


