{ mkDerivation, base, bytestring, containers, jsaddle-warp
, pretty-simple, reflex, reflex-dom-core, stdenv, tagsoup, text
}:
mkDerivation {
  pname = "htmltoreflexdom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers pretty-simple reflex reflex-dom-core
    tagsoup text
  ];
  executableHaskellDepends = [ base jsaddle-warp ];
  homepage = "htmltoreflexdom.github.io";
  description = "Convert HTML to Reflex-DOM code";
  license = stdenv.lib.licenses.bsd3;
}
