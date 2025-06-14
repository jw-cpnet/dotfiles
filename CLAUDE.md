# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository managed by [Chezmoi](https://www.chezmoi.io/). It contains configuration files for a Linux development environment (Arch Linux) with a focus on Emacs/Spacemacs as the primary editor and i3 as the window manager.

## Key Commands

### Chezmoi Management
```bash
# Apply changes to the system
chezmoi apply

# Add a file to chezmoi management
chezmoi add ~/.config/somefile

# Edit a managed file
chezmoi edit ~/.config/somefile

# See what would change
chezmoi diff
```

### Development Environment
- Primary editor: Emacs/Spacemacs (accessed via `ec` command)
- Proxy configuration: localhost:7890 (set in environment)
- SSH auth socket update: `~/.local/bin/update_ssh_auth_sock.sh` (for Spacemacs SSH integration)

## Architecture and Structure

### Encrypted Files
Sensitive configurations are encrypted with `.asc` extension:
- `private_dot_ssh/encrypted_*` - SSH configurations
- `dot_config/clash/encrypted_*` - Proxy profiles
- Various other sensitive configs

### Template Files
Files ending in `.tmpl` are Chezmoi templates that get processed based on machine-specific variables.

### Custom Spacemacs Layers

**Claude Layer** (`dot_spacemacs.d/layers/claude/`):
- Provides Claude AI integration in Emacs
- Key prefix: `SPC A` for Claude commands
- Buffers configured to use Emacs state by default

**JIRA Layer** (`dot_spacemacs.d/layers/jira/`):
- Automatic JIRA ticket prefix in git commits
- JIRA issue browsing within Emacs

### Key Configuration Areas
- `dot_spacemacs` & `dot_spacemacs.d/` - Emacs configuration
- `dot_config/i3/` - Window manager configuration
- `dot_bashrc` - Shell aliases and environment setup
- `dot_config/systemd/user/` - User services (like dunst notification daemon)

## Important Notes

- This is a configuration repository, not a software project - there are no build/test commands
- When modifying encrypted files, ensure they remain encrypted before committing
- The repository is branch-specific (current: laptop, main: main)
- Custom aliases redirect `vim`/`vi` to Emacs - be aware when suggesting editor commands