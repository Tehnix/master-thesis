if ((${#NIX_GHCJS[@]})); then
  buildDir="dist-ghcjs/build/x86_64-osx/ghcjs-0.2.1/frontend-0.1.0.0/c/frontend/build/frontend/frontend.jsexe"
  watchman-make \
    --make 'clear && cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all' \
    -p '**/*.hs' \
    -t ''
else
  nix-shell -A shells.ghcjs
fi


# Make Safari reload in the background by opening the index file.
#open -g file:///Users/tehnix/GitHub/Tehnix/master-thesis/src-reflex/dist-ghcjs/build/x86_64-osx/ghcjs-0.2.1/frontend-0.1.0.0/c/frontend/build/frontend/frontend.jsexe/index.html

# The full one-liner to build GHCJS inside nix.
#nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"

# Just build the GHCJS projects.
#cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all

# Run the closure optimizer on the outputted JS.
#&& ccjs $buildDir/rts.js --compilation_level=ADVANCED_OPTIMIZATIONS > $buildDir/rts.js \
#&& ccjs $buildDir/lib.js --compilation_level=ADVANCED_OPTIMIZATIONS > $buildDir/lib.js \
#&& ccjs $buildDir/out.js --compilation_level=ADVANCED_OPTIMIZATIONS > $buildDir/out.js \
#&& ccjs $buildDir/runmain.js --compilation_level=ADVANCED_OPTIMIZATIONS > $buildDir/runmain.js
