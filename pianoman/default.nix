{ stdenv, callPackage
, headlessTeamspeak, teamspeak_client
, pulseaudioLight, pavucontrol, moc
}:

stdenv.mkDerivation rec {
  name = "pianoman-${version}";
  src = ./.;

  installPhase = ''
    mkdir -pv $out/{bin,share/pianoman}

    install ./src/pianoman-config $out/bin/pianoman-config
    install ./src/pianoman-start  $out/bin/pianoman-start
    install ./src/pianoman-moc    $out/bin/pianoman-moc
    install ./misc/helpers.bash   $out/share/pianoman/helpers.bash
    install ./misc/config.bash    $out/share/pianoman/config.bash
    install ./misc/pulse.pa       $out/share/pianoman/pulse.pa

    export pulseConfig="$out/share/pianoman/pulse.pa"
    export pianomanConfig="$out/share/pianoman/config.bash"
    export pianomanHelpers="$out/share/pianoman/helpers.bash"

    substituteAllInPlace "$out/bin/pianoman-config"
    substituteAllInPlace "$out/bin/pianoman-start"
    substituteAllInPlace "$out/bin/pianoman-moc"
    substituteAllInPlace "$out/share/pianoman/helpers.bash"
    substituteAllInPlace "$out/share/pianoman/config.bash"
    substituteAllInPlace "$out/share/pianoman/pulse.pa"
  '';

  version                 = "0.0.1";
  pulseaudioBinary        = "${pulseaudioLight}/bin/pulseaudio";
  teamspeakBinary         = "${teamspeak_client}/bin/ts3client";
  headlessTeamspeakBinary = "${headlessTeamspeak}/bin/ts3client";
  mocBinary               = "${moc}/bin/mocp";

  pulseLoopbackSink = "ts";
  pulseInputSink = "music";

  meta = with stdenv.lib; {
    inherit version;
    homepage = "FIXME";
    downloadPage = "FIXME";
    description = "FIXME";
    longDescription = "FIXME";
    license = with licenses; [ mit ];
    maintainers = with maintainers; [ taktoa clever ];
    platforms = platforms.all;
  };
}
