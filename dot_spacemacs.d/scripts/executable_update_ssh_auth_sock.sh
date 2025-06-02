#!/bin/bash
new_auth_sock=$(echo $SSH_AUTH_SOCK)

# Prioritize non-gcr agent if available
if [[ -n "$new_auth_sock" && "$new_auth_sock" != "/run/user/1000/gcr/ssh" && -S "$new_auth_sock" ]]; then
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=$new_auth_sock|" ~/.spacemacs.d/.spacemacs.env
elif [[ -S "/run/user/1000/gcr/ssh" ]]; then # Fallback to gcr agent if it exists
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=/run/user/1000/gcr/ssh|" ~/.spacemacs.d/.spacemacs.env
elif [[ -S "/run/user/1000/gnupg/S.gpg-agent.ssh" ]]; then # Fallback to GPG agent SSH socket
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh|" ~/.spacemacs.d/.spacemacs.env
else
	echo "Warning: No valid SSH_AUTH_SOCK found. Using GPG agent as default." >&2
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh|" ~/.spacemacs.d/.spacemacs.env
fi
