alias clear 'clear; fish_greeting'
alias diff 'diff --color=auto'

# Check if hub's avaiable before setting alias
type -q hub; and alias git 'hub'

alias g↑ 'git push'
alias g↓ 'git pull'
alias gc 'git commit'
alias gd 'git diff'
alias gco 'git checkout'
alias gst 'git status'
alias gl 'git log'

alias vim 'echo using nvim...; sleep 0.1; nvim'
