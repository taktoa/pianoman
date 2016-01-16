{ buildEnv, teamspeak_client, plugins ? [] }:

buildEnv {
  name = "teamspeak_with_plugins";
  paths = plugins ++ [ teamspeak_client ];
  pathsToLink = [ "/lib" "/share" ];
  postBuild = ''
    rm $out/lib/teamspeak/ts3client
    cp -vi ${teamspeak_client}/lib/teamspeak/ts3client $out/lib/teamspeak/ts3client
    mkdir $out/bin
    substitute ${teamspeak_client}/bin/ts3client $out/bin/ts3client --replace ${teamspeak_client}/bin/.ts3client-wrapped $out/lib/teamspeak/ts3client
    chmod +x $out/bin/ts3client
  '';
}
