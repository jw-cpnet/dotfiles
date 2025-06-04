#!/bin/bash

# Function to find active SSH agent sockets
find_ssh_agent_socket() {
    local candidates=(
        "$SSH_AUTH_SOCK"  # Current environment variable (if set)
        "/run/user/$(id -u)/keyring/ssh"  # GNOME Keyring
        "/run/user/$(id -u)/ssh-agent"    # ssh-agent
        "/run/user/$(id -u)/gcr/ssh"      # GCR agent
    )

    # Check standard locations first
    for socket in "${candidates[@]}"; do
        # Skip if empty
        if [[ -z "$socket" ]]; then
            continue
        fi

        # Check if socket exists and is a socket
        if [[ -S "$socket" ]]; then
            echo "$socket"
            return 0
        fi
    done

    # Check for SSH agent sockets in /tmp with proper glob expansion
    for ssh_dir in /tmp/ssh-*/; do
        if [[ -d "$ssh_dir" ]]; then
            for agent_socket in "${ssh_dir}"agent.*; do
                if [[ -S "$agent_socket" ]]; then
                    echo "$agent_socket"
                    return 0
                fi
            done
        fi
    done

    return 1
}

# Find the best available SSH agent socket
new_auth_sock=$(find_ssh_agent_socket)

if [[ -n "$new_auth_sock" ]]; then
    # Create .spacemacs.env if it doesn't exist
    mkdir -p ~/.spacemacs.d
    touch ~/.spacemacs.d/.spacemacs.env

    # Update or add SSH_AUTH_SOCK in .spacemacs.env
    if grep -q "^SSH_AUTH_SOCK=" ~/.spacemacs.d/.spacemacs.env; then
        sed -i.bak -E "s|^SSH_AUTH_SOCK=.*|SSH_AUTH_SOCK=$new_auth_sock|" ~/.spacemacs.d/.spacemacs.env
    else
        echo "SSH_AUTH_SOCK=$new_auth_sock" >> ~/.spacemacs.d/.spacemacs.env
    fi

    echo "Updated SSH_AUTH_SOCK to: $new_auth_sock"
else
    echo "Error: No valid SSH_AUTH_SOCK found." >&2
    echo "Checked locations:" >&2
    echo "  - Current SSH_AUTH_SOCK: ${SSH_AUTH_SOCK:-'(not set)'}" >&2
    echo "  - /run/user/$(id -u)/keyring/ssh" >&2
    echo "  - /run/user/$(id -u)/ssh-agent" >&2
    echo "  - /tmp/ssh-*/agent.*" >&2
    echo "  - /run/user/$(id -u)/gcr/ssh" >&2
    exit 1
fi
