#
# ~/.bash_profile
#
# Sourced for login shells. Runs once per login; child interactive
# shells inherit the exported env (and re-source ~/.keychain/$HOSTNAME-sh
# from .bashrc as a fallback for terminals spawned without going through
# a login shell).

# Guard against keychain's recycled-PID trap. After a reboot the cached
# ~/.keychain/$HOSTNAME-sh still points at the previous (now dead) agent. Its
# stale SSH_AGENT_PID often gets reassigned to some unrelated process, so
# keychain's liveness check (kill -0 $PID) wrongly passes — it keeps the dead
# socket and every ssh-add fails with "Connection refused" forever. Probe the
# cached agent for real instead: source its env and ask ssh-add. Exit 2 means
# the socket is dead, so wipe the stale cache and clear the env, forcing
# keychain below to start a fresh agent.
kc_env="$HOME/.keychain/$HOSTNAME-sh"
if [ -f "$kc_env" ]; then
    . "$kc_env" >/dev/null
    ssh-add -l >/dev/null 2>&1
    # 0 = agent has keys, 1 = agent up but empty (both fine), 2 = can't connect.
    [ $? -eq 2 ] && rm -f "$HOME/.keychain/$HOSTNAME"-{sh,csh,fish}
    unset SSH_AUTH_SOCK SSH_AGENT_PID
fi
unset kc_env

# Build a list of SSH keys that actually exist, so machines with only
# id_rsa or only id_ed25519 don't get noisy "key not found" warnings.
ssh_keys=()
for k in id_rsa id_ed25519; do
    [ -f "$HOME/.ssh/$k" ] && ssh_keys+=("$k")
done
if [ ${#ssh_keys[@]} -gt 0 ]; then
    eval "$(keychain -q --eval "${ssh_keys[@]}")"
fi
unset ssh_keys k

[ -f ~/.bashrc ] && . ~/.bashrc
