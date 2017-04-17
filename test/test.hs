{-# LANGUAGE OverloadedStrings #-}

import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A

testtxt = 
  "( (S \n\
  \    (NP-SBJ-1 (NN Measuring) (NNS cups) )\n\
  \    (VP (MD may) \n\
  \      (ADVP-TMP (RB soon) )\n\
  \      (VP (VB be) \n\
  \        (VP (VBN replaced) \n\
  \          (NP (-NONE- *-1) )\n\
  \          (PP (IN by) \n\
  \            (NP-LGS (NNS tablespoons) ))\n\
  \          (PP-LOC (IN in) \n\
  \            (NP (DT the) (NN laundry) (NN room) )))))\n\
  \    (. .) ))"

main = 
  case A.parseOnly penntree testtxt of
    Left err -> putStrLn err
    Right tr -> do
      print tr
      putStrLn "=================="
      print (xformPennTree tr)
  
