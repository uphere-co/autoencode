{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NLP.SyntaxTree.Printer where

import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text as T  (concat,intercalate,replicate,take)
--
import NLP.SyntaxTree.Type

textprinter :: Int -> PennTree -> Text
textprinter n (PN _ lst) = T.intercalate "\n" (map (textprinter (n+4)) lst)
textprinter n (PL _ txt) = T.replicate n " " <> txt

pennTreePrint :: Int -> PennTree -> Text
pennTreePrint n (PN t lst) = "\n" <> fmttag <> T.concat (map (pennTreePrint (n+2)) lst)
  where fmttag = T.replicate n " " <> T.take 4 (t <> "    ") <> " "
pennTreePrint _ (PL t txt) = T.take 4 (t <> "    ") <> " " <> txt

btreePrint :: [Bool] -> (a -> Text) -> BinTree a -> Text
btreePrint _  s (BinLeaf txt) = s txt
btreePrint bs s (BinNode a b) = "\x252C\x2500" <> btreePrint (bs++[True]) s a <> "\n" <>
                                drawlines bs <> "\x2514\x2500" <> btreePrint (bs++[False]) s b
  where
    drawlines xs = foldMap (\case True -> "\x2502 " ; False -> "  ") xs


bntPrint :: [Bool] -> (e -> Text) -> (a -> Text) ->  BNTree e a -> Text
bntPrint _  _     lshow (BNTLeaf l)     = lshow l
bntPrint bs nshow lshow (BNTNode e a b) =
    "\x2299" <> nshow e <> "\n" <>
    drawlines bs <> "\x251C\x2500" <> bntPrint (bs++[True]) nshow lshow a <> "\n" <>
    drawlines bs <> "\x2514\x2500" <> bntPrint (bs++[False]) nshow lshow b
  where
    drawlines xs = foldMap (\case True -> "\x2502 " ; False -> "  ") xs

-- utility functions

convert :: BinTree Text -> BNTree Text Text
convert (BinLeaf a) = BNTLeaf a
convert (BinNode a b) = BNTNode "node" (convert a) (convert b)
