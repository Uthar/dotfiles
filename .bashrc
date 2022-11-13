wrap_parens () {
  local text=`eval $@`
  [ "$text" ] && echo -n "($text)"
}

git_branch() {
  wrap_parens git branch --show-current 2> /dev/null
}

fossil_branch() {
  wrap_parens \
    "fossil json branch list | jq -r '.payload.current | values' 2> /dev/null"
}

kube_ns() {
  [ -e ~/.kube/config ] || return;
  wrap_parens \
    kubectl config view --minify -o jsonpath='{..namespace}' 2> /dev/null
}

nix_shell() {
  [ $IN_NIX_SHELL ] && echo "(nix-shell)"
}

make_ps1() {
  echo -n '\[\033[01;038;2;0;211;208m\]'
  echo -n '\u@\h:\w '
  echo -n '$(git_branch)$(fossil_branch) '
  echo    '$(kube_ns)'
  echo -n '$(nix_shell)\$ '
  echo -n '\[\033[00m\]'
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
alias e="emacs ."
alias h=htop

shopt -s checkwinsize
shopt -s histappend
shopt -s autocd
stty -ixon -ixoff
eval "$(direnv hook bash)"

