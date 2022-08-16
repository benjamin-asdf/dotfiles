#!/bin/sh

# instal this btw
# xorg-fonts-misc

# XLOCK AND SSH-AGENT
#        If  you  use  ssh-agent(1) to avoid entering a passphrase every time you use one of your ssh(1) private keys, it's
#        good security practice to have ssh-agent forget the keys before you leave your terminal unattended.  That way,  an
#        attacker who takes over your terminal won't be able to use your private ssh keys to log in to other systems.  Once
#        you return to your terminal, you can enter the passphrase and re-add the keys to ssh-agent.  There are a couple of
#        ways in which xlock can help to automate this process.  Firstly, the startCmd option allows xlock to be configured
#        to run 'ssh-add -D' every time you lock the screen, so that your keys are automatically  deleted  from  ssh-agent.
#        If the passphrase on your ssh keys is the same as your password, then xlock can also be made to re-add the keys to
#        ssh-agent when you unlock the screen, via the pipepassCmd option.  This requires a bit of scripting, as  the  com‐
#        mand  must  read  your  password  from standard input and then automate the interaction with ssh-add to re-add the
#        keys.  There is an example of such a script in the xlock distribution - see etc/xlockssh*

# hm, I might do ssh-add -D at the end of the day

# xlock -startCmd "xrate-slow && ssh-add -D" -endCmd xrate-fast

# xlock -startCmd "xrate-slow" -endCmd "xrate-fast"

i3lock
