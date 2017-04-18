{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module NLP.SyntaxTree.Type.PropBank where

import           Control.Lens.TH
import           Data.Binary
import           Data.Char       (isDigit)
import           Data.Text       (Text)
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

instance Binary FunctionTag where
  put = putWord8 . fromIntegral . fromEnum
  get = (toEnum . fromIntegral) <$> getWord8 
  
identifyFuncTag :: Text -> FunctionTag
identifyFuncTag t = read (T.unpack (T.toUpper t))

data RoleNumber = RN Int
                | RM
                deriving (Show,Eq,Ord)

instance Binary RoleNumber where
  put (RN n) = putWord8 0 >> putWord8 (fromIntegral n)
  put RM = putWord8 1
  get = getWord8 >>= \x -> case x of
                             0 -> RN . fromIntegral <$> getWord8
                             1 -> return RM 

identifyRoleNumber :: Text -> RoleNumber
identifyRoleNumber t
  | T.all isDigit t = RN (read (T.unpack t))
  | T.toLower t == "m"        = RM
  | otherwise       = error ("identifyRoleNumber: " ++ T.unpack t)

data Role = Role { _role_description :: Text
                 , _role_function :: FunctionTag
                 , _role_number :: RoleNumber }
          deriving Show

instance Binary Role where
  put Role {..} =
    put _role_description >> put _role_function >> put _role_number
  get = do
    Role <$> get <*> get <*> get

makeLensesWith underscoreFields ''Role

emptyRole = Role "" LVB (RN 0)


