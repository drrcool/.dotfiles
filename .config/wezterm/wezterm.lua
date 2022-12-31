local wezterm = require("wezterm")

return {
	font = wezterm.font("Spleen 32x64"),
	font_size = 20,
	color_scheme = "Challenger Deep",
	window_background_opacity = 0.98,
	bold_brightens_ansi_colors = true,
	automatically_reload_config = true,
	hide_tab_bar_if_only_one_tab = true,
	use_fancy_tab_bar = true,
}
