

git_branch() {
    git branch --show-current 2> /dev/null
}

fossil_branch() {
    fossil json branch list | jq -r '.payload.current | values'
}

kube_ns() {
    kubectl config view --minify -o jsonpath='{..namespace}'
}

make_ps1() {
    echo '\u@\h:\w ($(git_branch)$(fossil_branch)) ($(kube_ns))\n\$ '
}

export PS1="$(make_ps1)"
export HISTCONTROL=ignoreboth
export HISTSIZE=1000
export HISTFILESIZE=2000
export EDITOR=emacs
export VISUAL=emacs
export TERM=xterm-256color

alias k=kubectl
alias n=nix
alias d=docker
alias f=fossil
alias g=git
alias x=exit
alias l="ls -lah"
alias ".."="cd .."
alias e="emacs ."
alias h=htop

shopt -s checkwinsize
shopt -s histappend
stty -ixon -ixoff
eval "$(direnv hook bash)"

