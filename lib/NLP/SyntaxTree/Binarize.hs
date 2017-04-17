{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module NLP.SyntaxTree.Binarize where

import           Data.Text (Text)
-- 
import           NLP.SyntaxTree.Type

testbtree :: BinTree Text
testbtree = BinNode (BinNode (BinLeaf "a") (BinLeaf "b")) (BinLeaf "c")

binarizeR :: PennTree -> BinTree Text
binarizeR (PL _ t)    = BinLeaf t
binarizeR (PN _ ys) = go ys
 where
   go []       = error "impossible"
   go (x:[])   = binarizeR x 
   go (x:xs)   = BinNode (binarizeR x) (go xs)
