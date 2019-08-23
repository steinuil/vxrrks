{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation rec {
  name = "aaaaaaa";

  buildInputs = [ clang m4 binutils pkgconfig ];

  shellHook = ''
    eval $(opam env)
  '';
}

