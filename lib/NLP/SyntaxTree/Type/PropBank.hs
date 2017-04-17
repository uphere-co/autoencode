{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.SyntaxTree.Type.PropBank where

import           Control.Lens.TH
import           Data.Binary
import           Data.Text      (Text)
import qualified Data.Text as T

-- documentation on github:propbank/propbank-documentation/data-format/frameset.dtd

data FunctionTag = EXT  -- ^ extent
                 | LOC  -- ^ location
                 | DIR  -- ^ direction
                 | NEG  -- ^ negation  (not in PREDITOR)
                 | MOD  -- ^ general modification
                 | ADV  -- ^ adverbial modification
                 | MNR  -- ^ manner
                 | PRD  -- ^ secondary predication
                 | REC  -- ^ reciprocal (eg herself, etc)
                 | TMP  -- ^ temporal
                 | PRP  -- ^ purpose
                 | PNC  -- ^ purpose no cause (no longer used)
                 | CAU  -- ^ cause
                 | ADJ  -- ^ adjectival (nouns only)
                 | COM  -- ^ comitative
                 | CXN  -- ^ constructional (for "compared to what" constructions for adjectivals)
                 | DIS  -- ^ discourse
                 | DSP  -- ^ direct speech
                 | GOL  -- ^ goal
                 | PAG  -- ^ prototypical agent (function tag for arg1)
                 | PPT  -- ^ prototypical patient (function tag for arg1)
                 | RCL  -- ^ relative clause (no longer used)
                 | SLC  -- ^ selectional constraint link
                 | VSP  -- ^ verb specific (function tag for numbered arguments)
                 | LVB  -- ^ light verb (for nouns only)
                 deriving (Show, Read, Eq, Ord, Enum)


identifyFuncTag :: Text -> FunctionTag
identifyFuncTag t = read (T.unpack (T.toUpper t))

 
data Role = Role { _role_description :: Text
                 , _role_function :: FunctionTag
                 , _role_number :: Int }
          deriving Show

makeLensesWith underscoreFields ''Role

emptyRole = Role "" LVB 0


