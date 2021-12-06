# Sample .bashrc for SuSE Linux
# Copyright (c) SuSE GmbH Nuernberg

# There are 3 different types of shells in bash: the login shell, normal shell
# and interactive shell. Login shells read ~/.profile and interactive shells
# read ~/.bashrc; in our setup, /etc/profile sources ~/.bashrc - thus all
# settings made here will also take effect in a login shell.
#
# NOTE: It is recommended to make language settings in ~/.profile rather than
# here, since multilingual X sessions would not work properly if LANG is over-
# ridden in every subshell.

# Some applications read the EDITOR variable to determine your favourite text
# editor. So uncomment the line below and enter the editor of your choice :-)
#export EDITOR=/usr/bin/vim
#export EDITOR=/usr/bin/mcedit

# For some news readers it makes sense to specify the NEWSSERVER variable here
#export NEWSSERVER=your.news.server

# If you want to use a Palm device with Linux, uncomment the two lines below.
# For some (older) Palm Pilots, you might need to set a lower baud rate
# e.g. 57600 or 38400; lowest is 9600 (very slow!)
#
#export PILOTPORT=/dev/pilot
#export PILOTRATE=115200

test -s ~/.alias && . ~/.alias || true


#export PATH=$PATH:~/.local/bin/
#export PYTHONPATH=$PYTHONPATH:~/.local/lib/python2.7/site-packages/

export PYTHONPATH=$PYTHONPATH:~/.local/lib/python3.6/site-packages/

#export PYTHONPATH=$PYTHONPATH:/home/mklett/w2d/lib/python3.6/site-packages/

export MODULEPATH=~/opt/modules

alias mpirun='/usr/lib64/mpi/gcc/openmpi2/bin/mpirun'

alias OmegaMaxEnt='/home/software/bin/OmegaMaxEnt'

alias emacs='emacs -nw' 

export OMP_NUM_THREADS=1


addlib () {
  if ! echo $CPATH | egrep -q "(^|:)$1/include($|:)" ; then
    export CPATH=$1/include${CPATH:+:$CPATH}
  fi
  if ! echo $LD_LIBRARY_PATH | egrep -q "(^|:)$1/lib($|:)" ; then
    export LD_LIBRARY_PATH=$1/lib:$1/lib64${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
  fi
  if ! echo $LIBRARY_PATH | egrep -q "(^|:)$1/lib($|:)" ; then
    export LIBRARY_PATH=$1/lib:$1/lib64${LIBRARY_PATH:+:$LIBRARY_PATH}
  fi
  if ! echo $PKG_CONFIG_PATH | egrep -q "(^|:)$1/lib($|:)" ; then
    export PKG_CONFIG_PATH=$1/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}
  fi
}

addpath () {
  if ! echo $PATH | egrep -q "(^|:)$1/bin($|:)" ; then
    export PATH=$1/bin${PATH:+:$PATH}
  fi
  if ! echo $MANPATH | egrep -q "(^|:)$1/man($|:)" ; then
    export MANPATH=$1/share/man${MANPATH:+:$MANPATH}
  fi
}

addactualpath () {
  if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
    export PATH=$1${PATH:+:$PATH}
  fi
  if ! echo $MANPATH | egrep -q "(^|:)$1/man($|:)" ; then
    export MANPATH=$1/share/man${MANPATH:+:$MANPATH}
  fi
}

addenv () {
  addlib $1
  addpath $1
}

if ! echo $PATH | egrep -q "(^|:)$HOME/bin($|:)" ; then
  export PATH=$HOME/bin:$PATH
fi

load_triqs3(){
    if [ -f ~/spack/share/spack/setup-env.sh ]; then
	. ~/spack/share/spack/setup-env.sh
	spack load gcc
    fi
    . ~/source/triqs_cdmft_auto_corr/install/share/triqs/triqsvars.sh
}    

load_triqs2(){
source /home/mklett/triqs2/share/triqsvars.sh
}    


load_w2d(){
addactualpath /home/mklett/Lanctanide/W2dynamics/w2dynamics.src/
addlib /usr/lib64/mpi/gcc/openmpi3/
export PYTHONPATH=$PYTHONPATH:/home/mklett/w2d/lib/python2.7/site-packages/
}    

gauss_w2d(){
export PATH=/usr/lib64/mpi/gcc/openmpi/bin:$PATH
export MANPATH=/usr/lib64/mpi/gcc/openmpi/share/man:$MANPATH
export LD_LIBRARY_PATH="/usr/lib64/mpi/gcc/openmpi/lib64:$LD_LIBRARY_PATH"
export PYTHONPATH="/usr/local/fkf/w2dynamics/W2-Dynamics_Python_27/lib/python2.7/site-packages"
export PATH=/usr/local/fkf/w2dynamics/W2-Dynamics_Python_27/bin/:$PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/fkf/nfft3/lib64/
export OMP_NUM_THREADS=1
}

if [ -f /etc/bash_completion.d/git-prompt.sh ]; then
    . /etc/bash_completion.d/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWSTASHSTATE=1
    export GIT_PS1_SHOWCOLORHINTS=1
    export GIT_PS1_SHOWUPSTREAM=auto
    PROMPT_COMMAND='__git_ps1 "\[$(ppwd)\]\u@\h:\w" "\\\$ "'
fi

export EDITOR='emacs -nw'

# netsettings
alias netsettings='git --git-dir="${XDG_DATA_HOME:-$HOME/.local/share}/netsettings/git" --work-tree="${HOME}"'
if declare -f _completion_loader &>/dev/null; then
	_completion_loader git
	if type __git_wrap__git_main &>/dev/null; then
		complete -o default -o nospace -F __git_wrap__git_main netsettings
	elif type _git &>/dev/null; then
		complete -o default -o nospace -F _git netsettings
	fi
fi
