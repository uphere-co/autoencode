import           Control.Applicative ( (*>),many)

import           Data.List.Split     (splitWhen)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A

 
main' = do
  putStrLn "interpreting propbank CoNLL"
  let fp = "/home/wavewave/repo/workspace/ontonotes/propbank-release/data/ontonotes/nw/wsj/23/wsj_2320.gold_skel"

  txt <- TIO.readFile fp
  let xs = T.lines txt
      xss = map T.words xs
      xsss = filter (not.null) (splitWhen null xss )
      
  mapM_ (\(x,y) -> print (x, map length y)) $ zip [0..] xsss

penntreefile = many (A.skipSpace *> penntree)
  
main = do
  let fp = "/home/wavewave/repo/workspace/Penn-tbank/MRG/WSJ/23/WSJ_2320.MRG"
  txt <- TIO.readFile fp
  case A.parseOnly penntreefile txt of
    Left err -> print err
    Right rs -> mapM_  print rs
