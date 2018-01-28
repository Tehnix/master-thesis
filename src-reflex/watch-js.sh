watchman-make \
    --make 'clear && cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all && open -g file:///Users/tehnix/GitHub/Tehnix/master-thesis/src-reflex/dist-ghcjs/build/x86_64-osx/ghcjs-0.2.1/frontend-0.1.0.0/c/frontend/build/frontend/frontend.jsexe/index.html' \
    -p '**/*.hs' \
    -t ''
