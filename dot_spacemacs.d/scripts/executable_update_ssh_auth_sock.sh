#!/bin/bash

# Get the value of SSH_AUTH_SOCK from the environment
new_auth_sock=$(echo $SSH_AUTH_SOCK)

# Check if SSH_AUTH_SOCK is set and valid
if [[ -z "$new_auth_sock" || ! -S "$new_auth_sock" ]]; then
    echo "Error: SSH_AUTH_SOCK is not set or invalid." >&2
    exit 1
fi

# Replace the line in the config file using sed (with backup)
sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=$new_auth_sock|" ~/.spacemacs.d/.spacemacs.env
