{-# LANGUAGE DataKinds #-}

module NLP.SyntaxTree.Type.PennTreebankII where


-- based on http://www.clips.ua.ac.be/pages/mbsp-tags

data POSTag = CC | CD | DT | EX | FW | IN | JJ | JJR | JJS | LS | MD | NN | NNS | NNP | NNPS | PDT | POS | PRP | PRPDollar
            | RB | RBR | RBS | RP | SYM | TO | UH | VB | VBZ | VBP | VBD | VBN | VBG | WDT | WP | WPDollar | WRB
            | MARKCLOSE | MARKCOMMA | MARKCOLON | MARKLP | MARKRP
            deriving (Show, Eq)

data ChunkTag = NP | PP | VP | ADVP | ADJP | SBAR | PRT | INTJ | PNP
              deriving (Show, Eq)

data IOBPrefix = I_ | B_ | O_ deriving (Show, Eq)

data RelationTag = R_SBJ | R_OBJ | R_PRD | R_TMP | R_CLR | R_LOC | R_DIR | R_EXT | R_PRP deriving (Show, Eq)

data AnchorTag = A1 | P1






 
            
