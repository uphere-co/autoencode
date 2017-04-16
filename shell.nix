{ pkgs ? (import <nixpkgs>{})
, uphere-nix-overlay
, symbolic
}:

with pkgs;

let hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
                 { inherit pkgs; };
    hsconfig2  = self: super: {
      "symbolic" = self.callPackage (import symbolic) {};
    };
    newhaskellPackages = haskellPackages.override {
      overrides = self: super: hsconfig self super // hsconfig2 self super;
    };
    
    hsenv = newhaskellPackages.ghcWithPackages (p: with p; [
              cabal-install
              xml-conduit split unordered-containers vector-algorithms storable-tuple
              tagged either
              mersenne-random
              math-functions
              hblas
              lbfgs
              MemoTrie lens
              language-c containers
	      llvm-general
	      QuickCheck
	      tasty
	      tasty-golden
	      tasty-hunit
	      tasty-quickcheck
	      tasty-smallcheck
              zenc
              p.symbolic
            ]);

in stdenv.mkDerivation {
     name = "ghc-shell";
     buildInputs = [ hsenv graphviz 
		   ];
     shellHook = ''
     '';
   }

