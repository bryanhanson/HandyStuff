
theme_poster <- function(base_size = 20) {
	update_geom_defaults("point", aes(size = 4))
	update_geom_defaults("line", aes(size = 1))
	update_geom_defaults("smooth", aes(size = 1))
	update_geom_defaults("pointrange", aes(size = 1))
	theme_grey(base_size)
	}
