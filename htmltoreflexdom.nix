{ mkDerivation, base, bytestring, containers, mtl, pretty, reflex
, reflex-dom, reflex-dom-core, stdenv, tagsoup, text
}:
mkDerivation {
  pname = "htmltoreflexdom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers mtl pretty reflex reflex-dom
    reflex-dom-core tagsoup text
  ];
  executableHaskellDepends = [ base ];
  homepage = "htmltoreflexdom.github.io";
  description = "Convert HTML to Reflex-DOM code";
  license = stdenv.lib.licenses.bsd3;
}
