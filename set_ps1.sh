# Repeat a string. E.g. replicate 3 "lu" gives "lululu"
function replicate() {
    local out
    printf -v out '%*s' "$1" ""
    echo "${out// /$2}"
}

# Produce a bunch of ─'s such that they can be used to pad $1 up to terminal
# width or max_width, whichever is smaller.
function make_padding() {
    local length=${#1}
    local width=80
    if [[ $COLUMNS -lt $width ]]; then
        width=$COLUMNS
    fi
    if [[ $length -lt $width ]]; then
        replicate $((width - length)) ─
    fi
}

function newline_if_not_at_start() {
    echo -e "\033[01;31m⮐\033[00m$(replicate $((COLUMNS - 2)) ' ')\r"
}

# Get current virtual env with a space after it. Returns "" if not in a virtual env.
function get_venv_str() {
    echo "${VIRTUAL_ENV:+"$(basename "$VIRTUAL_ENV") "}"
}

function get_debian_chroot_str() {
    echo "${debian_chroot:+($debian_chroot)}"
}

# Use like so:
#
#   export VIRTUAL_ENV_DISABLE_PROMPT="yes"  # Don't change PS1 in venv activate
#   set_ps1
#
function set_ps1() {
    # ps1_bars needs to be defined in global scope
    ps1_bars="$(replicate $((SHLVL - 1)) '│')"

    # Single quotes ensure that all the function calls and variable references
    # are not yet evaluated, they get expanded when the prompt is shown.

    PS1='$(newline_if_not_at_start)'
    PS1+='\[\033[01;33m\]$ps1_bars┌─\[\033[00m\] '
    PS1+='$(get_debian_chroot_str)'
    PS1+='\[\033[01;32m\]\u@\h\[\033[00m\] '
    PS1+='$(get_venv_str)'
    PS1+='\[\033[01;34m\]\w\[\033[00m\] '
    # Pass all variable components to make_padding for a consistent alignment
    PS1+='\[\033[01;33m\]─$(make_padding "$ps1_bars- $(get_debian_chroot_str)\u@\h \w $(get_venv_str) -")\[\033[00m\]\n'
    PS1+='\[\033[01;33m\]$ps1_bars│\[\033[00m\] '
}
