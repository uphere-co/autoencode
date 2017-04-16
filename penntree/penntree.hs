{-# LANGUAGE OverloadedStrings #-}

import NLP.SyntaxTree.Parser
import qualified Data.Attoparsec.Text as A

txt1 = 
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

txt2 =
  "( (S \n\
  \    (NP-SBJ-1 (DT The) (NNP Cincinnati) (NNS consumer-products) (NN giant) )\n\
  \    (VP (VBD got) \n\
  \      (VP (VBN clobbered) \n\
  \        (NP (-NONE- *-1) )\n\
  \        (ADVP-TMP \n\
  \          (NP (CD two) (NNS years) )\n\
  \          (RB ago) )\n\
  \        (PP-LOC (IN in)\n\ 
  \          (NP (NNP Japan) ))\n\
  \        (SBAR-TMP \n\
  \          (WHADVP (WRB when) )\n\
  \          (S \n\
  \            (NP-SBJ (NNP Kao) )\n\
  \            (VP (VBD introduced) \n\
  \              (NP \n\
  \                (NP (DT a) (JJ powerful) (NN detergent) )\n\
  \                (, ,) \n\
  \                (VP (VBN called) \n\
  \                  (S \n\
  \                    (NP-SBJ (-NONE- *) )\n\
  \                    (NP-PRD (NNP Attack) )))\n\
  \                (, ,) \n\
  \                (SBAR \n\
  \                  (WHNP-2 (WDT which) )\n\
  \                  (S \n\
  \                    (NP-SBJ (-NONE- *T*-2) )\n\
  \                    (VP \n\
  \                      (ADVP-MNR (RB quickly) )\n\
  \                      (VBD won) \n\
  \                      (NP \n\
  \                        (NP (DT a) \n\
  \                          (ADJP (CD 30) (NN %) )\n\
  \                          (NN stake) )\n\
  \                        (PP-LOC (IN in) \n\
  \                          (NP (DT the) (JJ Japanese) (NNS markets) ))))))))))))\n\
  \    (. .) ))"


main = 
  case A.parseOnly penntree txt2 of
    Left err -> putStrLn err
    Right tr -> do
      print tr
      putStrLn "=================="
      print (xformPennTree tr)
  
