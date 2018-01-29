if ((${#NIX_GHC[@]})); then
  watchman-make \
    --make 'clear && cabal new-build all -fno-bytecode' \
    -p '**/*.hs' \
    -t ''
else
  nix-shell -A shells.ghc
fi


# The full one-liner to build GHC inside nix.
#nix-shell -A shells.ghc --run "cabal new-build all"

# Just build the GHC projects.
#cabal new-build all
