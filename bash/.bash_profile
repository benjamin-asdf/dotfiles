#
# ~/.bash_profile
#
# Sourced for login shells. Runs once per login; child interactive
# shells inherit the exported env (and re-source ~/.keychain/$HOSTNAME-sh
# from .bashrc as a fallback for terminals spawned without going through
# a login shell).

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
