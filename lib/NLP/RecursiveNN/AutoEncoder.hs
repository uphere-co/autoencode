{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NLP.RecursiveNN.AutoEncoder where

import           Control.Monad.IO.Class             ( liftIO )
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.Join
import           Data.Bitraversable
import qualified Data.HashMap.Strict         as HM
import           Data.MemoTrie
import           Data.Monoid
import qualified Data.Vector.Storable        as VS
import           Data.Vector.Storable               ( Vector )
import           Data.Vector.Storable.Matrix
import           Data.Void
import qualified LLVM.General.AST            as AST
import           LLVM.General.AST.Type              ( float )
import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.JIT          ( LLVMRunT )
import           Symbolic.CodeGen.LLVM.Operation    ( LLVM, call, define, external, externf
                                                    , fadd, fdiv, fsub, fval, local, ret )
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
--
import           NLP.SyntaxTree.Type
--
import           Debug.Trace

type WVector = Vector Float
type family WVal a :: *
type instance WVal WVector = Float

type WMatrix = Matrix Float
type WExp = Exp Float
type WMExp = MExp Float

type NTree a = BNTree a ()


instance Bifunctor BNTree where
  bimap f g (BNTNode k l r) = BNTNode (f k) (bimap f g l) (bimap f g r)
  bimap f g (BNTLeaf x) = BNTLeaf (g x)

instance Bifoldable BNTree where
  bifoldMap f g (BNTNode k l r) = f k <> bifoldMap f g l <> bifoldMap f g r
  bifoldMap f g (BNTLeaf x) = g x

instance Bitraversable BNTree where
  bitraverse f g (BNTNode k l r) = BNTNode <$> f k <*> bitraverse f g l <*> bitraverse f g r
  bitraverse f g (BNTLeaf x) = BNTLeaf <$> g x

-- | this should be a member of Comonad instance.

duplicate :: Join BNTree a -> Join BNTree (Join BNTree a)
duplicate x@(Join y) =
  case y of
    BNTNode k l r -> Join (BNTNode x (runJoin (duplicate (Join l))) (runJoin (duplicate (Join r))))
    BNTLeaf _ -> Join (BNTLeaf x)

data AENode = AENode { aenode_autoenc :: AutoEncoder
                     , aenode_c1  :: WVector
                     , aenode_c2  :: WVector
                     }
              
data AutoEncoder = AutoEncoder { autoenc_dim :: Int
                               , autoenc_We  :: WMatrix
                               , autoenc_b   :: WVector
                               } 

data ADNode = ADNode { adnode_autodec :: AutoDecoder
                     , adnode_y  :: WVector
                     }

data AutoDecoder = AutoDecoder { autodec_dim :: Int
                               , autodec_Wd  :: WMatrix
                               , autodec_b   :: WVector
                               }

externFun :: LLVM ()
externFun = do
  external float "llvm.exp.f32" [(float, AST.Name "x")]
  define float "tanh" [(float, AST.Name "x")] $ do
    let xref = local (AST.Name "x")
    v <- fadd xref xref
    e <- call (externf (AST.Name "llvm.exp.f32")) [v]
    m <- fsub e (fval 1)
    d <- fadd e (fval 1)
    r <- fdiv m d
    ret r
  define float "tanh_1" [(float, AST.Name "x")] $ do
    let xref = local (AST.Name "x")
    v <- fadd xref xref
    e <- call (externf (AST.Name "llvm.exp.f32")) [v]
    einv <- fdiv (fval 1) e
    d1 <- fadd e einv
    d2 <- fadd d1 (fval 2)
    r <- fdiv (fval 4) d2
    ret r

enc :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
enc n =
  let idxi = ("i",1,n)
      idxj = ("j",1,n)
      idxk = ("k",1,n)
      idxI = ("I",1,2*n)
      -- 
      (c1,c2,we,be) = (iV ("c1",[idxi]),iV ("c2",[idxj]),iV ("we",[idxk, idxI]),iV ("be",[idxk]))
      -- 
      c = concat_ idxI [c1,c2]
      prd = sum_ [idxI] (mul [we, c])
      result = tanh_ [ add [prd, be] ]
  in (result, map mkV [("c1",[idxi]),("c2",[idxj]),("we",[idxk,idxI]),("be",[idxk])])


denc_dwe :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
denc_dwe n =
  let idxi = ("i",1,n)
      idxj = ("j",1,n)
      idxk = ("k",1,n)
      idxm = ("m",1,n)
      idxI = ("I",1,2*n)
      idxJ = ("J",1,2*n)
      --
      (r,_) = enc n
      dmap = HM.empty
      result = sdiff dmap (mkV ("we",[idxm,idxJ])) r
  in (result,map mkV [("c1",[idxi]),("c2",[idxj]),("we",[idxk,idxI]),("be",[idxk])])

denc_dbe :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
denc_dbe n =
  let idxi = ("i",1,n)
      idxj = ("j",1,n)
      idxk = ("k",1,n)
      idxm = ("m",1,n)
      idxI = ("I",1,2*n)
      --
      (r,_) = enc n
      dmap = HM.empty
      result = sdiff dmap (mkV ("be",[idxm])) r
  in (result,map mkV [("c1",[idxi]),("c2",[idxj]),("we",[idxk,idxI]),("be",[idxk])])


dec :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
dec n =
  let idxk = ("k",1,n)
      idxI = ("I",1,2*n)
      --
      (y,wd,bd) = (iV ("y",[idxk]),iV ("wd",[idxI,idxk]),iV ("bd",[idxI]))
      --
      prd = sum_ [idxk] (mul [wd, y])
      result = tanh_ [ add [prd, bd] ]
      --
  in (result, map mkV [("y",[idxk]),("wd",[idxI,idxk]),("bd",[idxI])])

ddec_dwd :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
ddec_dwd n =
  let idxk = ("k",1,n)
      idxI = ("I",1,2*n)
      idxJ = ("J",1,2*n)
      idxm = ("m",1,n)
      --
      (r,_) = dec n
      dmap = HM.empty
      result = sdiff dmap (mkV ("wd",[idxJ,idxm])) r
  in (result,map mkV [("y",[idxk]),("wd",[idxI,idxk]),("bd",[idxI])])

ddec_dbd :: (?expHash :: WExp :->: Hash) => Int -> (WMExp, [Variable])
ddec_dbd n =
  let idxk = ("k",1,n)
      idxI = ("I",1,2*n)
      idxJ = ("J",1,2*n)
      --
      (r,_) = dec n
      dmap = HM.empty
      result = sdiff dmap (mkV ("bd",[idxJ])) r
  in (result,map mkV [("y",[idxk]),("wd",[idxI,idxk]),("bd",[idxI])])



fullAST :: (?expHash :: WExp :->: Hash) => Int -> AST.Module
fullAST n = mkASTWithExt externFun
              [ ("encode" ,enc n)
              , ("dencdwe",denc_dwe n)
              , ("dencdbe",denc_dbe n)
              , ("decode" ,dec n)
              , ("ddecdwd",ddec_dwd n)
              , ("ddecdbd",ddec_dbd n)
              ]
 
encodeP :: AENode -> LLVMRunT IO WVector
encodeP AENode {..} = do
  let vc1 = aenode_c1
      vc2 = aenode_c2
      vwe = mat_content (autoenc_We aenode_autoenc)
      vb  = autoenc_b aenode_autoenc
      vr = VS.replicate 100 0    :: WVector
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "encode" [vc1,vc2,vwe,vb] fpr
  vr' <- liftIO $ VS.freeze mv
  return vr'
  
encode :: AutoEncoder
       -> BinTree WVector
       -> LLVMRunT IO (BNTree WVector WVector)
encode _ (BinLeaf x) = pure (BNTLeaf x)
encode autoenc (BinNode x y) = do
    x' <- encode autoenc x
    y' <- encode autoenc y
    r <- node x' y'
    return (BNTNode r x' y')
  where
    node x' y' = let vx = fromEither . rootElem $ x'
                     vy = fromEither . rootElem $ y'
                     ae = AENode autoenc vx vy
                 in encodeP ae


decodeP :: ADNode -> LLVMRunT IO (WVector, WVector)
decodeP ADNode {..} = do
  let dim = 100 --  autodec_dim adnode_autodec
      vy  = adnode_y
      vwd = mat_content (autodec_Wd adnode_autodec)
      vbd = autodec_b adnode_autodec
      vr = VS.replicate 200 0 :: WVector
  mv@(VS.MVector _ fpr) <- liftIO $ VS.thaw vr
  callFn "decode" [vy,vwd,vbd] fpr
  vr' <- liftIO $ VS.freeze mv
  let c1 = VS.slice 0 dim vr'
      c2 = VS.slice dim dim vr'
  return (c1,c2)

decode :: AutoDecoder
       -> BNTree WVector WVector
       -> LLVMRunT IO (BNTree (WVector,WVector) (WVector,WVector))
decode autodec bntr@(BNTNode v _ _) = go v bntr
  where 
    go v1 (BNTNode v0 x y) = do (c1,c2) <- decodeP (ADNode autodec v1)
                                BNTNode (v0,v1) <$> go c1 x <*> go c2 y
    go v1 (BNTLeaf v0) = pure (BNTLeaf (v0,v1))
decode _ (BNTLeaf v) = pure (BNTLeaf (v,v)) -- logically trivial encode-decoded. 


l2norm :: WVector -> WVector -> WVal WVector
l2norm v1 v2 = let vec_sub = VS.zipWith (*) v1 v2
               in VS.sum $ VS.zipWith (*) vec_sub vec_sub

-- unfolding RAE L^2 norm
l2unfoldingRAE:: AutoEncoder -> AutoDecoder -> BinTree WVector
              -> LLVMRunT IO (BNTree (WVal WVector) ())
l2unfoldingRAE ae ad bt  = do
  bte <- encode ae bt
  bted <- traverse (fmap Join . decode ad . runJoin) (duplicate (Join bte))
  return . bimap (bifoldl' const (+) 0 . bimap id (uncurry l2norm) . runJoin) (const ())
         . runJoin $ bted

-- differential 
