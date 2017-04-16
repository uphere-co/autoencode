{-# LANGUAGE DataKinds #-}

module NLP.SyntaxTree.Type.PennTreebankII where


-- based on http://www.clips.ua.ac.be/pages/mbsp-tags

data POSTag = CC          -- ^ conjunction, coordinating
            | CD          -- ^ cardinal number
            | DT          -- ^ determiner
            | EX          -- ^ existential there
            | FW          -- ^ foreign word 
            | IN          -- ^ conjunction, subordinating or preposition
            | JJ          -- ^ adjective
            | JJR         -- ^ adjective, comparative
            | JJS         -- ^ adjective, superlative
            | LS          -- ^ list item marker
            | MD          -- ^ verb, modal auxillary
            | NN          -- ^ noun, singular or mass
            | NNS         -- ^ noun, plural
            | NNP         -- ^ noun, proper singular
            | NNPS        -- ^ noun, proper plural
            | PDT         -- ^ predeterminer
            | POS         -- ^ possessive ending
            | PRP         -- ^ pronoun, personal
            | PRPDollar   -- ^ pronoun, possesive                (original PRP$)
            | RB          -- ^ adverb
            | RBR         -- ^ adverb, comparative
            | RBS         -- ^ adverb, superlative
            | RP          -- ^ adverb, particle
            | SYM         -- ^ symbol
            | TO          -- ^ infinitival to
            | UH          -- ^ interjection
            | VB          -- ^ verb, base form
            | VBZ         -- ^ verb, 3rd person singular present
            | VBP         -- ^ verb, non-3rd person singular present
            | VBD         -- ^ verb, past tense
            | VBN         -- ^ verb, past participle
            | VBG         -- ^ verb, gerund or present participle
            | WDT         -- ^ wh-determiner
            | WP          -- ^ wh-pronoun, personal
            | WPDollar    -- ^ wh-pronoun, possessive            (original WP$)
            | WRB         -- ^ wh-adverb
            | M_PERIOD    -- ^ punctuation mark, sentence closer (original .)
            | M_COMMA     -- ^ punctuation mark, comma           (original ,)
            | M_COLON     -- ^ punctuation mark, colon           (original :)
            | M_DQUOTE    -- ^ double quotation mark             (original '')
            | D_LRB       -- ^ left parenthesis                  (original -LRB-)
            | D_RRB       -- ^ right parentheis                  (original -RRB-)
            | D_NONE      -- ^ none                              (original -NONE-)
            deriving (Show, Eq, Ord, Enum)

data ChunkTag = NP        -- ^ noun phrase
              | PP        -- ^ prepositional phrase
              | VP        -- ^ verb phrase
              | ADVP      -- ^ adverb phrase
              | ADJP      -- ^ adjective phrase
              | SBAR      -- ^ subordinating conjunction
              | PRT       -- ^ particle 
              | INTJ      -- ^ interjection
              | PNP       -- ^ prepositional noun phrase 
              deriving (Show,Eq,Ord,Enum)

data IOBPrefix = I_       -- ^ inside the chunk 
               | B_       -- ^ inside the chunk, preceding word is part of a different chunk
               | O_       -- ^ not part of a chunk
               deriving (Show,Eq,Ord,Enum) 

data RelationTag = R_SBJ  -- ^ sentence subject
                 | R_OBJ  -- ^ sentence object
                 | R_PRD  -- ^ predicate
                 | R_TMP  -- ^ temporal
                 | R_CLR  -- ^ closely related 
                 | R_LOC  -- ^ location
                 | R_DIR  -- ^ direction
                 | R_EXT  -- ^ extent 
                 | R_PRP  -- ^ purpose
                 deriving (Show,Eq,Ord,Enum)

data AnchorTag = A1       -- ^ anchor chunks that corresponds to P1
               | P1       -- ^ PNP that corresponds to A1
               deriving (Show,Eq,Ord,Enum)





 
            
