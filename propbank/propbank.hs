{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative         ((*>),many)
import qualified Data.Attoparsec.Text as A
import           Data.Either                 (rights)
import           Data.Foldable
import           Data.List.Split             (splitWhen)
import           Data.Monoid
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Options.Applicative  as O
import           Text.Printf
--
import NLP.SyntaxTree.Type
import NLP.SyntaxTree.Type.PennTreebankII
import NLP.SyntaxTree.Parser
--
import Debug.Trace

format xs = T.pack (concatMap (\x -> printf "%20s " (T.unpack x)) xs)

penntreefile = many (A.skipSpace *> penntree)

combine xss y = 
  let lx = length xss
      ws = foldMap (\x->[x]) y
      ly = length ws
  in if lx == ly
     then Right (zipWith (:) ws xss)
     else Left (xss, y)

parseCoNLL fp = do
  txt <- TIO.readFile fp
  let xs = T.lines txt
      xss = map T.words xs
      xsss = filter (not.null) (splitWhen null xss )
  return (map (map (drop 4)) xsss)


parsePennTree fp = do
  txt <- TIO.readFile fp
  case A.parseOnly penntreefile txt of
    Left err -> error err
    Right rs -> return (map (pruneOutNone . xformPennTree) rs)

--   let fp = "wsj_2320.gold_skel"
--   let fp = "WSJ_2320.MRG"

data Config = Config { propfile :: FilePath
                     , pennfile :: FilePath }
  
config :: O.Parser Config
config = Config <$> O.strOption (O.long "prop" <> O.short 'p' <> O.help "PropBank file")
                <*> O.strOption (O.long "penn" <> O.short 'n' <> O.help "Penn Treebank file")

main = do
  c <- O.execParser (O.info config O.fullDesc)
  xsss <- parseCoNLL (propfile c)
  ys <- parsePennTree (pennfile c)
  mapM_ (\ts -> mapM_ (TIO.putStrLn . format) ts >> putStrLn "=================") (rights (zipWith combine xsss ys))


