{ mkDerivation, base, containers, fetchgit, hashable, log-domain
, primitive, stdenv, unordered-containers, vector
}:
mkDerivation {
  pname = "semirings";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/semirings.git";
    sha256 = "12g7yhrxz928frk8lv9jjmkvy30zdmyah5zfljyl5rwqxzd12br1";
    rev = "b4cbe40acb75680fa6ea902657e443735e7bb513";
  };
  libraryHaskellDepends = [
    base containers hashable log-domain primitive unordered-containers
    vector
  ];
  homepage = "https://github.com/chessai/semirings#readme";
  description = "semirings";
  license = stdenv.lib.licenses.bsd3;
}
