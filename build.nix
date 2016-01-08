{ pkgs ? import <nixpkgs> {} }:


let cp = pkgs.callPackage;
in (rec {
  headlessTeamspeak = cp ./headless-teamspeak {};
  pianoman          = cp ./pianoman { inherit headlessTeamspeak; };
}).pianoman
