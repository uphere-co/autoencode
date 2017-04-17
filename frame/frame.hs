{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Options.Applicative as O
import qualified Text.Taggy.Lens as X


data Role = Role { _descr :: Text
                 , _roleF :: Text
                 , _roleN :: Text }
          deriving Show

makeLenses ''Role

emptyRole = Role "" "" ""

data Config = Config { framefile :: FilePath }

config :: O.Parser Config
config = Config <$> O.strOption (O.long "frame" <> O.short 'f' <> O.help "frame xml file")

main = do
  c <- O.execParser (O.info config O.fullDesc)
  -- let fp = "take.xml"
  txt <- TLIO.readFile (framefile c)
  let me = txt ^? X.html . X.allNamed (only "frameset") . X.allNamed (only "predicate")
  case me of
    Nothing -> error "parsing error"
    Just e -> do
      let n = e ^. X.attrs . ix "lemma"
          rolesets = e ^.. X.allNamed (only "roleset")
      TIO.putStrLn ("lemma: " <> n)
          
      flip mapM_ rolesets $ \roleset -> do
        let rolesetid = roleset ^. X.attrs . ix "id"
            roles = roleset ^?! X.allNamed (only "roles")   -- roles should exist uniquely
            roles' = flip map (roles ^.. X.allNamed (only "role") . X.attrs) $ \r ->
                       emptyRole &~ do descr .= (r ^. ix "descr")
                                       roleF .= (r ^. ix "f")
                                       roleN .= (r ^. ix "n")
        print (rolesetid,roles')


