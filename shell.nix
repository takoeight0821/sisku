{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [
    haskell-language-server
    rust-analyzer
  ];
}
