module Printer where

import Data.Monoid

import Parser

guardQuickQuote :: [PhrasingContent] -> [PhrasingContent]
guardQuickQuote [] = []
guardQuickQuote l  = tail $ foldr f [] $ zip (qq:l) (l ++ [qq])
  where
    qq = QuickQuote 0 '*' []
    f (x@(QuickQuote _ _ _), y@(QuickQuote _ _ _)) r = [x, TextContent ""] ++ r
    f (x, y) r = x:r

toHaskell :: [PhrasingContent] -> String
toHaskell l  = case guardQuickQuote l of
  []     -> ""
  (x:xs) -> (showContent x) <> (foldr (<>) "" $ zipWith f (x:xs) xs)
  where
    f (QuickQuote _ _ _) y   = showContent y
    f x y@(QuickQuote _ _ _) = showContent y
    f x y = "<>" <> (showContent y)

{-
  where
    f []     = ""
    f (x@(QuickQuote _ _ _):xs) = (leftQQ x) <> (showContent x) <> (rightQQ x) <> (f xs)
    f (x:xs) = "<>" <> (showContent x) <> (f xs)
-}

showContent :: PhrasingContent -> String
showContent (TextContent s)      = "\"" <> s <> "\""
showContent (QuickQuote n k cs)  = leftQQ <> (toHaskell cs) <> rightQQ
  where leftQQ  = "<" <> (replicate n k)
        rightQQ = (replicate n k) <> ">"
showContent (AppQuote name args) = "(" <> x <> ")"
  where
    x     = unwords (name:args')
    args' = map toHaskell $ map (:[]) args
