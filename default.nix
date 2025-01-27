{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.mkDerivation {
  pname = "mmsyn2-array";
  version = "0.3.1.1";
  src = ./.;
  libraryHaskellDepends = [ pkgs.haskellPackages.base ];
  homepage = "https://hackage.haskell.org/package/mmsyn2-array";
  description = "A library that can be used for multiple Ord a => a -> b transformations";
  license = pkgs.lib.licenses.mit;
}
