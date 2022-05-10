#' Add labels on top of bars or boxes
#'
#' @description
#' Adds text labels on top of bars or boxes of a `ggplot2`-based plot.
#' Useful for annotating statistical analysis information.
#'
#' @details
#' `plot_stats` detects the maximum plotted value of each data group (be it bar, box, error bar or individual data point),
#' and plots labels above it.
#'
#' The supplied plot needs to be based on `ggplot2` and to have a discrete x scale. The supplied `data.frame` needs to have a
#' `character` field with the labels to be plotted (whose name is specified in the `labels` argument) and `factor` field(s)
#' for the groups in which each label belongs.
#'
#' @param plot Plot onto which to add labels. Requires a discrete x scale, optionally dodged by "fill" aesthetic.
#' @param d `data.frame` containing the labels to be plotted.
#' Must have categorical fields corresponding to the data used for the original plot.
#' @param labels Character. The name of the column with the labels to be plotted.
#' @param position Character, either "identity" or "dodge". Sets where (horizontally) the labels should be plotted. Defaults to "identity".
#' If plot is dodged, "dodge" plots labels above each bar or box. "identity" plots them above the middle of each group of bars or boxes.
#' If plot is not dodged, position is automatically set to "identity".
#' @param size Numeric. Sets the size of labels. Defaults to 1.
#' @param y.adj Numeric. Adjusts the height of labels above each bar, box or group. Defaults to 0.
#' Positive sets it higher, negative sets it lower.
#'
#' @return A plot based on `ggplot2`.
#'
#' @import data.table
#' @import ggplot2
#'
#' @export
#'
plot_stats <- function(plot,d,labels,position="identity",size=1,y.adj=0) {

  if(!inherits(layer_scales(plot)$x,"ScaleDiscrete")) {
    stop("Plot's x axis does not have discrete values.")
  }

  x.name <- ggiraphExtra::getMapping(plot$mapping,"x")
  fill.name <- ggiraphExtra::getMapping(plot$mapping,"fill")
  y.name <- ggiraphExtra::getMapping(plot$mapping,"y")

  if(!is.null(plot$data[[x.name]])) {
    x.levels <- levels(plot$data[[x.name]])
    x.exists <- TRUE
  } else {
    x.exists <- FALSE
    x.name <- NULL
  }

  if(!is.null(fill.name)) {
    fill.levels <- levels(plot$data[[fill.name]])
    fill.exists <- TRUE
  } else {
    fill.exists <- FALSE
  }

  if(fill.exists) {
    if(position=="identity") {
      text.position <- position_identity()
    } else {
      text.position <- position_dodge(width=.get_dodge_width(plot))
    }
  } else {
    if(position=="identity") {
      text.position <- position_identity()
    } else {
      warning("There is no fill aesthetic to dodge by. position is set to 'identity'.")
      position <- "identity"
      text.position <- position_identity()
    }
  }

  stats.labels <- .get_top_y(plot,position=position,x.name=x.name,fill.name=fill.name)

  stats.labels[d,
               plotted.text:=get(..labels),
               on=c(x.name,fill.name)]

  updated.plot <- plot +
    geom_text(data=stats.labels,mapping=aes(y=max.value,label=plotted.text,vjust=-1*y.adj-0.5,hjust=0.5),
              position=text.position,
              size=5*size)

  return(updated.plot)

}

#' @import data.table
#' @import ggplot2
#'
.get_top_y <- function(plot,position,x.name,fill.name) {
  # Get plotted data from ggplot_build(plot)$data
  # Get original data from plot$data (only for factor level names)

  if(is.null(x.name)) {
    fill.levels <- levels(plot$data[[fill.name]])
    top.y <- data.table(fill=fill.levels)
    top.y[,fill:=factor(fill,levels=fill.levels)]
    setnames(top.y,"fill",fill.name)

  } else if(is.null(fill.name)) {
    x.levels <- levels(plot$data[[x.name]])
    top.y <- data.table(x=x.levels)
    top.y[,x:=factor(x,levels=x.levels)]
    setnames(top.y,"x",x.name)

  } else {
    x.levels <- levels(plot$data[[x.name]])
    fill.levels <- levels(plot$data[[fill.name]])
    top.y <- data.table::CJ(x=x.levels,fill=fill.levels,sorted=FALSE) |> as.data.table()
    top.y[,`:=`(x = factor(x,levels=x.levels),
                            fill = factor(fill,levels=fill.levels))]
    setnames(top.y,c("x","fill"),c(x.name,fill.name))
  }

  top.y[,group:=.I]

  plot.rendering <- ggplot_build(plot)$data

  top.y.positions <- lapply(X=plot.rendering,FUN=function(dat) {
    dat <- as.data.table(dat)
    dat <- dat[,.(max.value=max(.SD)),
               by="group",
               .SDcols=grep("*ymax*",names(dat),value=TRUE)]
  }) |> rbindlist()

  top.y.positions <- top.y.positions[,.SD[max.value==max(max.value)],by="group"]

  top.y[top.y.positions,
        max.value:=max.value,
        on="group"]

  if(position=="identity" & !is.null(fill.name)) {
    top.y[,max.value:=max(max.value),by=x.name]
  }

  top.y[,group:=NULL]

  return(top.y)

}

.get_dodge_width <- function(plot) {

  if(!is.null(plot$layers[[1]]$position$width)) {
    dodge.width <- plot$layers[[1]]$position$width
  } else {
    dodge.width <- plot$layers[[1]]$position$dodge.width
  }

  return(dodge.width)
}
