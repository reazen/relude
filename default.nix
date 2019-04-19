with import <nixpkgs> {};
mkShell {
  buildInputs = [ nodejs-10_x ] ++ (with ocamlPackages_4_02; [ ocaml ninja merlin ]);
  shellHook = ''
    export PATH="`pwd`/node_modules/.bin:$PATH"
  '';
}
