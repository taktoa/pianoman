{ pkgs ? import <nixpkgs> {} }:


let
  callPackage = pkgs.newScope self;
  self = rec {
    jsoncpp = pkgs.jsoncpp.overrideDerivation (old: {
        cmakeFlags = [
            "-DBUILD_SHARED_LIBS=ON"
            "-DBUILD_STATIC_LIBS=OFF"
            "-DJSONCPP_WITH_CMAKE_PACKAGE=ON"
        ];
    });
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
