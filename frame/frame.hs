{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
-- import qualified Text.Taggy as 
import qualified Text.Taggy.Lens as X


main = do
  putStrLn "frame parsing"
  let fp = "take.xml"
  txt <- TLIO.readFile fp
  let me = txt ^? X.html . X.allNamed (only "frameset") . X.allNamed (only "predicate")
  case me of
    Nothing -> error "parsing error"
    Just e -> do
      print $ (e ^. X.attrs . ix "lemma")
