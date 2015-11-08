#!/bin/bash

# Fix SSH password bug
# http://askubuntu.com/questions/362280/enter-ssh-passphrase-once
SSH_ENV=$HOME/.ssh/environment

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add
}

if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

killall mjpg_streamer

echo "Security stopped."

# ## Log into second computer and start security
# ssh syllogismrxs@192.168.1.24 /bin/bash <<EOF
# /home/syllogismrxs/stop-security.sh
# EOF
