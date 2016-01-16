{ pkgs ? import <nixpkgs> {} }:


let
  callPackage = pkgs.newScope self;
  self = rec {
    headlessTeamspeak = callPackage ./headless-teamspeak {};
    pianoman          = callPackage ./pianoman {};
    plugin            = callPackage ./plugin {};
  };
in self
