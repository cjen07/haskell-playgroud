module IsbnVerifier (isbn) where

import Data.List

isbn :: String -> Bool
isbn xs
  | length xs == 10 = isbn' 0 10 xs
  | length xs /= 13 = False
  | xs !! 1 /= '-' = False
  | xs !! 5 /= '-' = False
  | xs !! 11 /= '-' = False
  | otherwise = isbn' 0 10 (filter (/= '-') xs)

isbn' :: Int -> Int -> String -> Bool
isbn' sum 1 (x:[]) =
  case (isbn'' x True) of
    Just m -> mod (sum + m) 11 == 0
    Nothing -> False
isbn' sum n (x:xs) =
  case (isbn'' x False) of
    Just m -> isbn' (sum + m*n) (n-1) xs
    Nothing -> False

isbn'' :: Char -> Bool -> Maybe Int
isbn'' c b =
  case findIndex (== c) ['0'..'9'] of
    Just i -> Just i
    Nothing ->
      case (b && c == 'X') of
        True -> Just 10
        False -> Nothing




