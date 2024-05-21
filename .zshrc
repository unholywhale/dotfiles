function ec() {
    if [[ "$(emacsclient -e '(length (frame-list))')" = 1 ]]; then
        emacsclient -c -n "$@"
    else
        emacsclient -n "$@"
        emacsclient -e "(select-frame-set-input-focus (selected-frame))"
    fi
}

alias emacs=ec
