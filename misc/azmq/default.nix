{ stdenv, cmake, fetchFromGitHub, boost, zeromq }:

stdenv.mkDerivation {
  name = "azmq";
  src = fetchFromGitHub {
    owner = "zeromq";
    repo = "azmq";
    rev = "08713a17ea4458bb8d5042100aa70d9fade38941";
    sha256 = "110cda505bjy8i0hggij0zgzblxm3llqx0kw88wbchm9ag463r4i";
  };
  buildInputs = [ cmake boost zeromq ];
}
