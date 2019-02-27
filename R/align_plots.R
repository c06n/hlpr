#' Align plots in one column
#'
#' Arranges plots from \code{ggplot} into one column. The use case is to have
#' several plots which need to be arranged along a time line. With other approaches,
#' labels and ticks on the vertical axes are taken into account, because they are
#' part of the plot. With the \code{grid} package, it is possible to align them
#' along the x-axis.
#'
#' @param p A \code{list} of plots
#' @param save Should the outcome be saved to a \code{.png}-file?
#' @param fname Provide a file name for the save file. If none is provided, one
#' is produced from the plot list name.
#'
#' @import grDevices
#' @import grid
#' @import gridExtra
#'
#' @export align_plots
#'
align_plots <- function(p, save = FALSE, fname = NULL) {

  g <- list()
  label <- lapply(names(p), paste0)

  for (i in seq(p)) {

    g[[i]] <- ggplotGrob(p[[i]])
    colnames(g[[i]]) <- paste0(seq_len(ncol(g[[i]])))

  }

  if (save) {

    if (is.null(fname)) fname <- paste0(label, "_aligned.png")
    grDevices::png(filename = fname,
        width = 2000, height = 1400, units = "px",
        res = 200,
        bg = "white")

  } else {

    stopifnot(names(dev.list()[1]) == "RStudioGD")
    grid::grid.newpage()

  }

  print(
    grid::grid.draw(do.call(gridExtra::gtable_combine, c(g, along = 2)))
  )

  if (save) grDevices::dev.off()

}
