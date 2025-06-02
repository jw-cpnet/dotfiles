local wezterm = require 'wezterm'
local config = {}

-- Basic configurations
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Disable fancy tab style
config.use_fancy_tab_bar = false

-- Hide the tab bar if there is only one tab
config.hide_tab_bar_if_only_one_tab = true

-- Set window opacity
config.window_background_opacity = 0.8

-- VIM like modal keybinding
local modal = wezterm.plugin.require("https://github.com/MLFlexer/modal.wezterm")
modal.apply_to_config(config)

return config
