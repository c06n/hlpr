#' Multiplot
#'
#' Arranges plots into a layout. If the layout is something like \code{matrix(c(1,2,3,3),
#' nrow = 2, byrow = TRUE)}, then plot 1 will go in the upper left, 2 will go in the
#' upper right, and 3 will go all the way across the bottom.
#'
#' @param plotlist A list of \code{ggplot}-objects
#' @param cols Number of columns in layout
#' @param layout Number of columns in layout
#' @param ... \code{ggplot}-objects
#'
#' @details Credits go to \href{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}{Cookbook for R}
#'
#' @import ggplot2
#' @import grid
#'
#' @export mplot
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # This example uses the ChickWeight dataset, which comes with ggplot2
#' # First plot
#' p1 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) +
#'  geom_line() +
#'  ggtitle("Growth curve for individual chicks")
#'
#' # Second plot
#' p2 <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) +
#' geom_point(alpha=.3) +
#' geom_smooth(alpha=.2, size=1) +
#' ggtitle("Fitted growth curve per diet")
#'
#' mplot(p1, p2, cols=2)
#'
mplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
