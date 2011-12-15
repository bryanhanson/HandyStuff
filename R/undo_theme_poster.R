
undo_theme_poster <- function() {
	update_geom_defaults("point", aes(size = 2))
	update_geom_defaults("line", aes(size = 0.5))
	update_geom_defaults("smooth", aes(size = 0.5))
	update_geom_defaults("pointrange", aes(size = 0.5))
	theme_gray(14)
}
