
theme_poster <- function(base_size = 24) {
	update_geom_defaults("point", aes(size = 4))
	update_geom_defaults("line", aes(size = 2))
	theme_grey(base_size)
	}