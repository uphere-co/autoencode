{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens
import           Data.Binary               (encode,decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                 (sort)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Options.Applicative as O
import           System.Directory
import           System.FilePath
import           System.IO
import qualified Text.Taggy.Lens as X
--
import           NLP.SyntaxTree.Type.PropBank



data Config = Config { framedir :: FilePath
                     , outputfile :: FilePath 
                     }

config :: O.Parser Config
config = Config <$> O.strOption (O.long "dir" <> O.short 'd' <> O.help "frame xml file directory")
                <*> O.strOption (O.long "output" <> O.short 'o' <> O.help "output bin file directory")

main :: IO ()
main = do
  c <- O.execParser (O.info config O.fullDesc)
  let dir = framedir c 
  filelst <- map (dir </>) . sort . filter (\x -> takeExtension x == ".xml") <$> getDirectoryContents dir
  
  withFile (outputfile c) WriteMode $ \h -> do
    lst <- mapM (process h) filelst
    BL.hPutStr h (encode lst)
    

process :: Handle -> FilePath -> IO (Text,[(Text,[Role])]) 
process h fp = do
  putStrLn fp
  txt <- TLIO.readFile fp
  let me = txt ^? X.html . X.allNamed (only "frameset") . X.allNamed (only "predicate")
  case me of
    Nothing -> error ("parsing error in " ++ fp)
    Just e -> do
      let n = e ^. X.attrs . ix "lemma"
          rolesets = e ^.. X.allNamed (only "roleset")
          rs = flip map rolesets $ \roleset -> 
                 let rolesetid = roleset ^. X.attrs . ix "id"
                     -- roles should exist uniquely                     
                     roles = roleset ^?! X.allNamed (only "roles")   
                     roles' = flip map (roles ^.. X.allNamed (only "role") . X.attrs) $ \r ->
                                emptyRole &~ do description .= (r ^. ix "descr")
                                                function    .= identifyFuncTag (r ^. ix "f")
                                                number      .= identifyRoleNumber (r ^. ix "n")
                 in (rolesetid,roles')
      return (n,rs)


main' = do
  lbstr <- BL.readFile "test.bin"
  let rs = decode lbstr :: [(Text,[(Text,[Role])])]
  mapM_ print rs

