{ pkgs ? (import <nixpkgs>{})
, nlp-types
, uphere-nix-overlay
, symbolic
}:

with pkgs;

let hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
                 { inherit pkgs; };
    hsconfig2  = self: super: {
      "symbolic" = self.callPackage (import symbolic) {};
      "nlp-types" = self.callPackage (import nlp-types) {};
      hexpat-lens = haskell.lib.doJailbreak super.hexpat-lens;
    };
    newhaskellPackages = haskellPackages.override {
      overrides = self: super: hsconfig self super // hsconfig2 self super;
    };

    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              xml-conduit split unordered-containers vector-algorithms storable-tuple
              tagged either
              containers
              hblas
              lbfgs
              language-c 
              MemoTrie lens
              mersenne-random
              math-functions
              llvm-general
              QuickCheck
              taggy-lens
              tasty
              tasty-golden
              tasty-hunit
              tasty-quickcheck
              tasty-smallcheck
              zenc
              p.nlp-types
              p.symbolic
            ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz 
		   ];
     shellHook = ''
     '';
   }

