#!/bin/bash
new_auth_sock=$(echo $SSH_AUTH_SOCK)

# Prioritize non-gcr agent if available
if [[ "$new_auth_sock" != "/run/user/1001/gcr/ssh" && -S "$new_auth_sock" ]]; then
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=$new_auth_sock|" ~/.spacemacs.d/.spacemacs.env
elif [[ -S "/run/user/1001/gcr/ssh" ]]; then # Fallback to gcr agent if it exists
	sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=/run/user/1001/gcr/ssh|" ~/.spacemacs.d/.spacemacs.env
else
	echo "Error: No valid SSH_AUTH_SOCK found." >&2
	exit 1
fi
