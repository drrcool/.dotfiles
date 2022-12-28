local wezterm = require("wezterm")

return {
	font = wezterm.font("PragmataProMonoLiga Nerd Font"),
	font_size = 16,
	color_scheme = "carbonfox",
	window_background_opacity = 0.99,
	bold_brightens_ansi_colors = true,
	automatically_reload_config = true,
	hide_tab_bar_if_only_one_tab = true,
	use_fancy_tab_bar = true,
}
