{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ( (*>),many)
import           Data.Either         (rights)
import           Data.Foldable
import           Data.List.Split     (splitWhen)
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Text.Printf
--
import NLP.SyntaxTree.Type
import NLP.SyntaxTree.Type.PennTreebankII
import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A
--
import Debug.Trace

format xs = T.pack (concatMap (\x -> printf "%20s " (T.unpack x)) xs)

task1 = do
  putStrLn "interpreting propbank CoNLL"
  let fp = "wsj_2320.gold_skel"

  txt <- TIO.readFile fp
  let xs = T.lines txt
      xss = map T.words xs
      xsss = filter (not.null) (splitWhen null xss )
  return (map (map (drop 4)) xsss)
  -- mapM_ (\(x,y) -> print (x, map length y)) $ zip [0..] xsss

penntreefile = many (A.skipSpace *> penntree)


task2 = do
  let fp = "WSJ_2320.MRG"
  txt <- TIO.readFile fp
  case A.parseOnly penntreefile txt of
    Left err -> error err
    Right rs -> return (map (pruneOutNone . xformPennTree) rs)

combine xss y = 
  let lx = length xss
      ws = foldMap (\x->[x]) y
      ly = length ws
  in if lx == ly
     then Right (zipWith (:) ws xss)
     else Left (xss, y)

main = do
  xsss <- task1
  ys <- task2
  mapM_ (\ts -> mapM_ (TIO.putStrLn . format) ts >> putStrLn "=================") (rights (zipWith combine xsss ys))


