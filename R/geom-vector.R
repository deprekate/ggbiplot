GeomVector <- ggproto("GeomVector", Geom,
  #objname <- "vector"
  #default_stat <- function(.) StatIdentity
  required_aes = c("x", "y"),
  default_aes = function(.) aes(
    xbegin = 0, 
    ybegin = 0, 
    colour = "black", 
    size = 0.5, 
    linetype = 1, 
    alpha = NA
  ),
  #guide_geom = function(.) "segment"
  extra_params = c("na.rm", "garrow"),
  draw_panel = function(data, scales, coordinates, garrow) {
    if (empty(data)) return(zeroGrob())
	
    # Drop rows where (x, y) is nearly equal to (xbegin, ybegin)
    r2 <- with(data, (x - xbegin)^2 + (y - ybegin)^2)
    data <- subset(data, Vectorize(all.equal)(r2, 0) != TRUE)

    segment <- transform(data, 
      xend = x, yend = y, x = xbegin, y = ybegin,
      xbegin = NULL, ybegin = NULL
    )
    gList(
    	GeomSegment$draw_panel(segment, scales, coordinates, arrow=garrow)
    )
  }
)

#' Arrows
#'
#' @param arrow specification for arrow heads, as created by arrow()
#' @export
geom_vector <- function (mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, garrow = grid::arrow(length = unit(1/3, "picas")), ...) {
	layer(
		geom = GeomVector, mapping = mapping,
		data = data, stat = stat, position = position,
		params = list(na.rm = na.rm, garrow = garrow),
		...
    )
}
