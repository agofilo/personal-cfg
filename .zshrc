export PATH="/usr/local/sbin:/Applications/jakarta-jmeter-2.4/bin:$PATH"

source ~/.zsh/colors.zsh
source ~/.zsh/setopt.zsh
source ~/.zsh/exports.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/completion.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/bindkeys.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/history.zsh
source ~/.zsh/zsh_hooks.zsh
source ~/.zsh/plugins/command_coloring.zsh


[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

export EC2_HOME=~/.ec2
export PATH=$PATH:$EC2_HOME/bin
export EC2_PRIVATE_KEY=~/.ec2/certs/pk-OKXLFMO76CJ5XDZ6ZF3ZL3VRSS2WODZY.pem
export EC2_CERT=~/.ec2/certs/cert-OKXLFMO76CJ5XDZ6ZF3ZL3VRSS2WODZY.pem

unsetopt correct_all
unsetopt correct