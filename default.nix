{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation rec {
  name = "aaaaaaa";

  buildInputs = [ clang m4 binutils ];

  shellHook = ''
    eval $(opam env)
  '';
}

