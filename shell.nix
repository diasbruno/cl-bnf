{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [sbcl];
  buildInputs = with pkgs.lispPackages; [quicklisp];
}
