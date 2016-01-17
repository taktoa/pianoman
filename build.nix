{ pkgs ? import <nixpkgs> {} }:


let
  callPackage = pkgs.newScope self;
  self = rec {
    headlessTeamspeak = callPackage ./headless-teamspeak {};
    pianoman          = callPackage ./pianoman {
      teamspeak_client = teamspeak_with_plugins;
      headlessTeamspeak = headlessteamspeak_with_plugins;
    };
    plugin            = callPackage ./plugin {};
    headlessteamspeak_with_plugins = callPackage ./teamspeak/teamspeak-with-plugins.nix {
      teamspeak_client = headlessTeamspeak;
      plugins = [ plugin ];
    };
    teamspeak_with_plugins = callPackage ./teamspeak/teamspeak-with-plugins.nix {
      plugins = [ plugin ];
    };
  };
in self
