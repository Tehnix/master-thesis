cabal() {
    if [[ $@ == "configure" ]]; then
        command cabal new-configure | more
    else
        command cabal "$@"
    fi
}
