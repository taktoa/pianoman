{ pkgs ? import <nixpkgs> {} }:


let
  callPackage = pkgs.newScope self;
  self = rec {
    headlessTeamspeak = callPackage ./headless-teamspeak {};
    pianoman          = callPackage ./pianoman {};
    plugin            = callPackage ./plugin {};
    teamspeak_with_plugins = callPackage ./teamspeak/teamspeak-with-plugins.nix {
      #teamspeak_client = headlessTeamspeak;
      plugins = [ plugin ];
    };
  };
in self
