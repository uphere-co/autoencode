import           Control.Applicative ( (*>),many)
import          Data.Traversable (sequenceA)
import           Data.List.Split     (splitWhen)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
--
import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A


task1 = do
  putStrLn "interpreting propbank CoNLL"
  let fp = "wsj_2320.gold_skel"

  txt <- TIO.readFile fp
  let xs = T.lines txt
      xss = map T.words xs
      xsss = filter (not.null) (splitWhen null xss )
      
  mapM_ (\(x,y) -> print (x, map length y)) $ zip [0..] xsss

penntreefile = many (A.skipSpace *> penntree)

task2 = do
  let fp = "WSJ_2320.MRG"
  txt <- TIO.readFile fp
  case A.parseOnly penntreefile txt of
    Left err -> print err
    Right rs -> do
      
      flip mapM_ rs $ \r -> do
        print r
        print (xformPennTree r)
      print (length rs)

main = task1 >> task2
