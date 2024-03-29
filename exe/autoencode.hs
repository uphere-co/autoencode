{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- import GHC.IO.Encoding                             ( setLocaleEncoding, utf8 )

import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.Trans.Reader        ( runReaderT )
import qualified Data.Attoparsec.Text       as A
import           Data.Bifoldable
import           Data.Bifunctor.Join
import qualified Data.ByteString.Char8      as B
import           Data.Foldable
import           Data.Hashable                     ( hash )
import qualified Data.HashMap.Strict        as HM
import           Data.MemoTrie                     ( trie )
import           Data.Text                         ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Vector.Storable              ( Vector )
import qualified Data.Vector.Storable       as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import qualified LLVM.General.AST           as AST
import           LLVM.General.AST.Type             ( float )
import           LLVM.General.Context              ( withContext )
import           System.Environment
import           System.Random.Mersenne

import           Text.Printf
--
import           Symbolic.CodeGen.LLVM.JIT
import           Symbolic.CodeGen.LLVM.Operation
import           Symbolic.CodeGen.LLVM.Run
import           Symbolic.Differential
import           Symbolic.Eval
import           Symbolic.Predefined
import           Symbolic.Print
import           Symbolic.Type
import           Symbolic.Util
--
import           Data.Vector.Storable.Matrix
import           NLP.RecursiveNN.AutoEncoder
import           NLP.SyntaxTree.Binarize
import           NLP.SyntaxTree.Parser
import           NLP.SyntaxTree.Printer
import           NLP.SyntaxTree.Regularize
import           NLP.SyntaxTree.Type
import           NLP.WordVector.Vectorize


prepareData :: IO (Vector Float)
prepareData = do
    bstr <- B.readFile "/data/groups/uphere/randomtest/randomtest.dat"
    v :: Vector Float <- B.useAsCString bstr $ \cstr -> do
      nstr <- mallocBytes 4000000
      copyBytes nstr cstr 4000000
      fptr <- castForeignPtr <$> newForeignPtr_ nstr
      return (V.unsafeFromForeignPtr0 fptr 1000000)
    return v

getVectorizedTree :: WordVectorMap -> PennTree -> (BinTree Text, Maybe (BinTree (Vector Float)))
getVectorizedTree wvm tr = (btr, traverse (\w -> (fmap snd . HM.lookup w . wvmap) wvm) btr)
  where
    btr0  = binarizeR tr
    btr   = regularize btr0


-- let p (v0,v1) = "ab"
-- printer :: BNTree (WVector,WVector) (WVector,WVector) -> Text

{-
printer1 = bntPrint [] p p where p = T.pack . show . V.take 4

printer2 = bntPrint [] p p where p (v0,v1) = "ab"
-}

printer3 :: BNTree Float () -> Text
printer3 = bntPrint [] p q
  where p n = T.pack (show n)
        q _ = "leaf"

main' :: IO ()
main' = do
    -- setLocaleEncoding utf8
    args <- getArgs 
    let !n1 = read (args !! 0) :: Int
        !n2 = read (args !! 1) :: Int
    v <- V.map (\x -> x - 0.5) <$> prepareData
    putStrLn "data prepared"
    let we = Mat (100,200) . V.slice 0 20000 . V.map (/100.0) $ v
        be = V.slice 20000 100 . V.map (/100.0) $ v
        wd = Mat (200,100) . V.slice 20100 20000 . V.map (/10.0) $ v
        bd = V.slice 40100 200 . V.map (/50.0) $ v
    let ?expHash = trie hash        
    let autoenc = AutoEncoder 100 we be
        autodec = AutoDecoder 100 wd bd
    txt <- TIO.readFile "/data/groups/uphere/LDC2003T05_POS/LDC2003T05_parsed1.pos" -- "parsed.txt"
    (_,wvm) <- createWordVectorMap "/data/groups/uphere/tmp/nlp-data/word2vec-result-20150501/vectors100statmt.bin"
    let p' = penntree <* A.skipSpace 
        r = A.parseOnly (A.many1 p') txt
    case r of
      Left err -> print err
      Right lst -> do
        withContext $ \context ->
          flip runReaderT context $
            compileNRun ["encode", "decode"] (fullAST 100) $
              
              forM_ ((drop n1 . take n2) lst) $ \tr -> do
                let (_btr,mvtr) = getVectorizedTree wvm tr
                forM_ mvtr $ \vtr -> do
                  r <- l2unfoldingRAE autoenc autodec vtr
                  liftIO $ TIO.putStrLn (printer3 r)
                  -- liftIO $ print r
                  liftIO $ print (bifoldl' (+) const 0 r)
                  {-
                  enc <- encode autoenc vtr
                  dec <- decode autodec enc
                  liftIO $ do
                    putStrLn "================"
                    TIO.putStrLn $ printer1 enc
                    putStrLn "----------------"
                    TIO.putStrLn $ printer2 dec
                  rdec <- traverse (fmap Join . decode autodec . runJoin) (duplicate (Join enc))
                  liftIO $ print rdec
                  return () -}
                  
                  -- liftIO $ do
                  --   putStrLn "****************"
                  --   print rdec
                    -- TIO.putStrLn . bntPrint [] printer (const "(no leaf)") $ rdec
        return ()

 


comparison = do
  let idxm = ("m",1,2)
      idxJ = ("J",1,4)
  let ?expHash = trie hash
      ?functionMap = HM.fromList [ ("tanh", \[x] -> tanh x), ("tanh_1", \[x] -> 1/((cosh x)*(cosh x))) ]
  let (r,_) = dec 2
      dmap = HM.empty -- HM.fromList [("y",["wd"])]
  let expdiff = sdiff dmap (mkV ("wd",[idxJ,idxm])) r
  printf "r= %s\n" ((prettyPrint . exp2RExp) r :: String)
  printf "diff = %s\n" ((prettyPrint . exp2RExp) expdiff :: String)

  putStrLn "Interpreter Evaluation:"
  let v_wd  = tV [0.1,0.2,0.3,0.4,0.3,0.2,0.1,0.2]
      v_wd' = tV [0.1+0.001,0.2,0.3,0.4,0.3,0.2,0.1,0.2]
      v_y   = tV [1.3, 2.5]
      v_bd  = tV [1.2,1.1,0.9,0.8]
      args0 = mkA [ ("wd",v_wd), ("y" ,v_y), ("bd",v_bd) ]
      args1 = mkA [ ("wd",v_wd'), ("y", v_y), ("bd",v_bd) ]

  forM_ [1..4] $ \iI -> do
    let iptI = [("I",iI)]
        r0 = seval args0 iptI r
        r1 = seval args1 iptI r
        dr = r1 - r0
    printf "I=%d, r0 = %f, dr = %f \n" iI r0 dr

  forM_ [1..4] $ \iI -> do
    let iJ = 1
        m = 1
    let ipt = [("I",iI),("J",iJ),("m",m)]
    printf "diff(I=%d,J=%d,m=%d) = %f \n" iI iJ m (seval args0 ipt expdiff)

  let n=2; m=2*n
  
  putStrLn "LLVM code result:"
  withContext $ \context ->
    flip runReaderT context $ do
      compileNRun ["encode","dencdwe","decode","ddecdwd","ddecdbd"] (fullAST n) $ do
        dwd <- mutateWith (V.replicate (m*m*n) 0) $ \fpr -> 
          callFn "ddecdwd" [v_y,v_wd,v_bd] fpr
        liftIO $ print dwd


generation = do  
  mtgen <- newMTGen Nothing
  let n = 4; m = 2*n
  let ?expHash = trie hash
  
  vwd <- tV . take (m*n) . map realToFrac <$> (randoms mtgen :: IO [Double])
  vbd <- tV . take m     . map realToFrac <$> (randoms mtgen :: IO [Double])
  vy  <- tV . take n     . map realToFrac <$> (randoms mtgen :: IO [Double])

  vwe <- tV . take (n*m) . map realToFrac <$> (randoms mtgen :: IO [Double])
  vbe <- tV . take n     . map realToFrac <$> (randoms mtgen :: IO [Double])
  vc1 <- tV . take n     . map realToFrac <$> (randoms mtgen :: IO [Double])
  vc2 <- tV . take n     . map realToFrac <$> (randoms mtgen :: IO [Double])


  withContext $ \context ->
    flip runReaderT context $ do
      compileNRun ["encode","dencdwe","dencdbe","decode","ddecdwd","ddecdbd"] (fullAST n) $ do
        liftIO $ putStrLn "ddec/dwd:"
        dw <- mutateWith (V.replicate (m*m*n) 0) $ \fpr -> 
          callFn "ddecdwd" [vy,vwd,vbd] fpr
        liftIO $ print dw
        --
        liftIO $ putStrLn "ddec/dbd:"
        db <- mutateWith (V.replicate (m*m) 0) $ \fpr -> 
          callFn "ddecdbd" [vy,vwd,vbd] fpr
        liftIO $ print db
        --
        liftIO $ putStrLn "denc/dwe:"
        ew <- mutateWith (V.replicate (n*m*n) 0) $ \fpr -> 
          callFn "dencdwe" [vc1,vc2,vwe,vbe] fpr
        liftIO $ print ew
        --
        liftIO $ putStrLn "denc/dbe:"
        eb <- mutateWith (V.replicate (n*n) 0) $ \fpr -> 
          callFn "dencdbe" [vc1,vc2,vwe,vbe] fpr
        liftIO $ print eb
  

main = comparison -- generate
