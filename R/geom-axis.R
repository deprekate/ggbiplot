GeomAxis <- ggproto("GeomAxis", Geom,
	required_aes = c("x", "y"),
	default_aes = aes(
		xbegin = 0, 
		ybegin = 0, 
		colour = "#832424FF", 
		label = NULL, 
		size = 0.5, 
		linetype = 1, 
		alpha = NA,
		textsize = 3,
		family = "",
		fontface = 1,
		lineheight = 1.2
	),
	extra_params = c("na.rm", "garrow"),
	draw_panel = function(data, panel_params, coord, garrow) {
		if (empty(data)) return(zeroGrob())
		#if (!is.linear(coord)) {
		#	warning("geom_axis does not work properly with non-linear coordinates.")
		#}
		
		vec <- transform(data,textsize = NULL, family = NULL, fontface = NULL, lineheight = NULL)

		gList(
			if (!is.null(data$label)) {
				text <- transform(data, 
					size = textsize,
					angle = (180/pi) * atan(y / x),
					hjust = 0.5 * (1 - 1.25 * sign(x)),
					vjust = 0.5
				)
				GeomText$draw_panel(text, panel_params, coord)				
			},
			ggbiplot2:::GeomVector$draw_panel(vec, panel_params, coord, garrow)
    	)
  	}
)

#' Axis arrows with optional text labels.
#'
#' @param arrow specification for arrow heads, as created by arrow()
#' @export
pca_axis <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, garrow = grid::arrow(length = unit(1/3, "picas")), 
  ...) {
      layer(
        geom = GeomAxis, mapping = mapping,
        data = data, stat = stat, position = position,
        #arrow = arrow,
        #show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, garrow = garrow, ...),
        ...
      )
}
