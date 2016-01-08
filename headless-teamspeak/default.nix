{ stdenv, callPackage
, teamspeak_client, libpulseaudio
, headlessTeamspeakQt5 ? (callPackage ./qt5.nix {})
}:

let ts3Version = (builtins.parseDrvName teamspeak_client.name).version;
    rename = stdenv.lib.setName "teamspeak-headless-client-${ts3Version}";
    inputs = { inherit libpulseaudio;
               qt55          = { qtbase = headlessTeamspeakQt5; };
               freetype      = headlessTeamspeakQt5;
               # We don't want to provide any of the X libraries,
               # but we need to give a path
               xorg          = { libSM       = headlessTeamspeakQt5; 
                                 libICE      = headlessTeamspeakQt5; 
                                 libxcb      = headlessTeamspeakQt5;
                                 libXrender  = headlessTeamspeakQt5; 
                                 libXrandr   = headlessTeamspeakQt5; 
                                 libXfixes   = headlessTeamspeakQt5; 
                                 libXcursor  = headlessTeamspeakQt5; 
                                 libXinerama = headlessTeamspeakQt5;
                                 libXext     = headlessTeamspeakQt5;
                                 libX11      = headlessTeamspeakQt5;
                               };
             };
in rename ((teamspeak_client.override inputs).overrideDerivation (old: {
  postInstall = old.installPhase + ''
      mv $out/bin/ts3client $out/bin/headless-teamspeak
      mv $out/lib/teamspeak $out/lib/headless-teamspeak
      rm $out/bin/.ts3client-wrapped
      ln -s $out/lib/headless-teamspeak/ts3client \
            $out/bin/.ts3client-wrapped
      rm -rf $out/share
  '';
}))
