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
  echo -n '\[\033[01;32m\]\u@\h\[\033[00m\]'
  echo -n '\[\033[01m\]:\[\033[00m\]'
  echo -n '\[\033[01;34m\]\w\[\033[00m\]'
  echo -n ' \[\033[38;5;214m\]$(git_branch)$(fossil_branch)\[\033[00m\]'
  echo -n ' \[\033[38;5;75m\]$(kube_ns)\[\033[00m\]'
  echo -n '\n'
  echo -n '\[\033[01;34m\]$(nix_shell)\[\033[00m\]'
  echo -n '\[\033[01m\]\$\[\033[00m\] '
}

export PS1="$(make_ps1)"
export HISTCONTROL=ignoreboth
export HISTSIZE=100000
export HISTFILESIZE=100000
export EDITOR=emacs
export VISUAL=emacs
export TERM=xterm-256color

alias k=kubectl
alias n=nix
alias d=docker
alias f=fossil
alias g=git
alias x=exit
alias l="ls -vlah --color=auto --group-directories-first"
alias ls="ls --color=auto --group-directories-first"
alias e="emacs"
alias s=systemctl
alias j=journalctl

shopt -s checkwinsize
shopt -s histappend
shopt -s autocd
stty -ixon -ixoff
eval "$(direnv hook bash)"

# Naprawia zgniłe kolorki w screen
#
# Bez tego miałem problemy z wyświetlaniem kolorów w emacs -nw. Były zupełnie
# inne niż w tmux i średnio czytelne. Wciąż są inne przy modus-operandi-tinted,
# ale są już OK przy modus-operandi.
unset COLORTERM
