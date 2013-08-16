# Flow diagram functions
# Taken from article by Paul Murrell
# "Drawing diagrams with R"
# The R Journal Vol. 1/1, May 2009  

tableBox <- function(labels, x=.5, y=.5) {
	nlabel <- length(labels)
	tablevp <- viewport(x=x, y=y,
			width=max(stringWidth(labels)) +
					unit(4, "mm"),
			height=unit(nlabel, "lines"))
	pushViewport(tablevp)
	grid.roundrect()
	if (nlabel > 1) {
		for (i in 1:(nlabel - 1)) {
			fill <- c("white")
			grid.clip(y=unit(i, "lines"), just="bottom")
			grid.roundrect(gp=gpar(fill=fill))
		}
	}
	grid.clip()
	grid.text(labels,y=unit(nlabel:1 - .5, "lines"),just="centre")
	popViewport()
}

boxGrob <- function(labels, x=.5, y=.5) {
	grob(labels=labels, x=x, y=y, cl="box")
}
drawDetails.box <- function(x, ...) {
	tableBox(x$labels, x$x, x$y)
}
xDetails.box <- function(x, theta) {
	nlines <- length(x$labels)
	height <- unit(nlines, "lines")
	width <- unit(4, "mm") + max(stringWidth(x$labels))
	grobX(roundrectGrob(x=x$x, y=x$y, width=width, height=height),
			theta)
}
yDetails.box <- function(x, theta) {
	nlines <- length(x$labels)
	height <- unit(nlines, "lines")
	width <- unit(4, "mm") + max(stringWidth(x$labels))
	grobY(rectGrob(x=x$x, y=x$y, width=width, height=height),
			theta)
}
