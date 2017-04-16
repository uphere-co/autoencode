{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ( (*>),many)
import           Data.Foldable
import           Data.List.Split     (splitWhen)
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import NLP.SyntaxTree.Type
import NLP.SyntaxTree.Type.PennTreebankII
import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A
--
import Debug.Trace

task1 = do
  putStrLn "interpreting propbank CoNLL"
  let fp = "wsj_2320.gold_skel"

  txt <- TIO.readFile fp
  let xs = T.lines txt
      xss = map T.words xs
      xsss = filter (not.null) (splitWhen null xss )
      
  mapM_ (\(x,y) -> print (x, map length y)) $ zip [0..] xsss

penntreefile = many (A.skipSpace *> penntree)

isNone (PL D_NONE _) = True
isNone _             = False

pruneOutNone :: Monoid m => PennTreeGen ChunkTag POSTag m -> PennTreeGen ChunkTag POSTag m
pruneOutNone (PN t xs) = let xs' = (filter (not . isNone) . map pruneOutNone) xs
                         in if null xs' then PL D_NONE mempty else PN t xs' 
pruneOutNone x = x 

task2 = do
  let fp = "WSJ_2320.MRG"
  txt <- TIO.readFile fp
  case A.parseOnly penntreefile txt of
    Left err -> print err
    Right rs -> do
      
      flip mapM_ rs $ \r -> do
        print r
        let r' = (pruneOutNone . xformPennTree) r
        print r'
        TIO.putStrLn (foldMap (<> " ") r')
        putStrLn "================================="
      print (length rs)

main = task1 >> task2
