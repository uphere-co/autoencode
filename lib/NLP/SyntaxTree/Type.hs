{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.SyntaxTree.Type where

import           Data.Text                   (Text)

data PennTreeGen c p a = PN c [PennTreeGen c p a]
                       | PL p a
                   deriving (Show, Functor, Foldable, Traversable)


                            
type PennTree = PennTreeGen Text Text Text

data BinTree a = BinNode (BinTree a) (BinTree a)
               | BinLeaf a
               deriving (Functor, Foldable, Traversable) 

deriving instance (Show a) => Show (BinTree a)

data BNTree e a = BNTNode e (BNTree e a) (BNTree e a)
                | BNTLeaf a
                deriving (Functor, Foldable, Traversable)

deriving instance (Show e,Show a) => Show (BNTree e a)

newtype BNTreeS a = BNTreeS { unBNTreeS :: BNTree a a }

instance Functor BNTreeS where
  fmap f (BNTreeS (BNTNode x y z)) = BNTreeS (BNTNode (f x)
                                                      ((unBNTreeS . fmap f . BNTreeS) y)
                                                      ((unBNTreeS . fmap f . BNTreeS) z))
  fmap f (BNTreeS (BNTLeaf x))     = BNTreeS (BNTLeaf (f x))

rootElem :: BNTree e a -> Either e a
rootElem (BNTLeaf x) = Right x
rootElem (BNTNode x _ _) = Left x

fromEither :: Either a a -> a
fromEither = either id id

binTree2BNTree :: BinTree a -> BNTree () a
binTree2BNTree (BinNode x y) = BNTNode () (binTree2BNTree x) (binTree2BNTree y)
binTree2BNTree (BinLeaf x)   = BNTLeaf x

