To build

```
$ nix-shell shell.nix --argstr uphere-nix-overlay (uphere-nix-overlay) --argstr symbolic (symbolic)
$ cabal sandbox init
$ cabal install
```
where `(uphere-nix-overlay)` and `(symbolic)` are the directories where the corresponding repo is
located.

To use propbank
```
$ cabal exec -- runhaskell propbank/propbank.hs -n (penntreebank file) -p (propbank file)
```
We have additional `-t` option for showing results in transposed format.

To use frame
```
$ cabal exec -- runhakell frame/frame.hs -d (frame xml dir) -o (output binary file)
```


* Brief summary of source files
 - exe/acctest.hs
 
 - exe/autoencode.hs

 - exe/bench.c

 - exe/distance.c

 - exe/extract.hs
    Extracts only sentences from 'English Gigaword' file ("LDC2003T05")
    link ; https://catalog.ldc.upenn.edu/desc/addenda/LDC2003T05.gz
    output file ; sentences$(n).txt

 - exe/fetch.hs
    Fetches US-patent metadata from the uspto database
    link ;  https://bulkdata.uspto.gov/data2/patent/grant/redbook/fulltext/2016/
    output files ; "ipg160105.xml", "ipg160112.xml", "ipg160119.xml", "ipg160126.xml", "ipg160202.xml"
      , "ipg160209.xml", "ipg160216.xml", "ipg160223.xml", "ipg160301.xml", "ipg160308.xml"
      , "ipg160315.xml", "ipg160322.xml", "ipg160329.xml", "ipg160405.xml"

 - exe/lbfgs.hs
    testing L-BFGS module
    link ; https://hackage.haskell.org/package/lbfgs 

 - exe/parse.hs
    Parse the uspto metadata given in XML, and extracts the contents of <description> tag
    input file ; "ipg160105.xml"

 - exe/run.hs
    A script to run a parser bash script, exe/parse.sh, which transforms the senetnces, in sentences$(n).txt files, into the tree-structured parsed data, stored in LDC2003T05_parsed$(n).pos.
    input files ; sentences$(n).txt
    output files ; LDC2003T05_parsed$(n).pos 
    ! Currently, the bash script file exe/parse.sh is not in the repository ! 

 - exe/tf-idf.hs
 - exe/treebank.hs
 - exe/wordvecparser.hs


 - lib/NLP/RecursiveNN/AutoEncoder.hs

 - lib/NLP/SyntaxTree/Binarize.hs
 - lib/NLP/SyntaxTree/Parser.hs
 - lib/NLP/SyntaxTree/Printer.hs
 - lib/NLP/SyntaxTree/Regularize.hs
 - lib/NLP/SyntaxTree/Type.hs
 
 - lib/NLP/WordVector/Parser.hs
 - lib/NLP/WordVector/Vectorize.hs
