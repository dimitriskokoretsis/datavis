#' Box plots of grouped data with optional mean value
#'
#' Create standard box plots of grouped data (line: median, box: 25 to 75 percentiles, whiskers: 1.5*IQR (interquartile range)).
#' Can also calculate and show arithmetic or geometric mean. Individual data points are included by default.
#'
#' @param d `data.frame` with data to be plotted.
#' @param x Character. The name of the column to be used for the x axis (categorical data).
#' Defaults to `NULL`, but then `color.group` needs to be supplied.
#' @param y Character. The name of the column to be used for the y axis (numeric data).
#' @param color.group Character. The name of the column to be used for color grouping (categorical data).
#' Defaults to `NULL`, but then `x` needs to be supplied.
#' @param x.axis Character. The title of the x axis. If `NULL`, x axis title is given the value of `x`.
#' Defaults to `NULL`.
#' @param y.axis Character. The title of the y axis. If `NULL`, y axis title is given the value of `y`.
#' Defaults to `NULL`.
#' @param legend.title Character. The title of the legend. If `NULL`, legend title is given the value of `color.group`.
#' Defaults to `NULL`.
#' @param x.order Character vector of length equal to the number of x categories.
#' Sets the order of categories in the x axis. Defaults to alphabetical order.
#' @param group.order Character vector of length equal to the number of color grouping categories.
#' Sets the order of color groups. Defaults to alphabetical order.
#' @param x.first Character. Places a specific x axis category first. Ignored if `x.order` is supplied.
#' Defaults to `NULL`.
#' @param group.first Character. Places a specific color group category first.
#' Ignored if `group.order` is supplied. Defaults to `NULL`.
#' @param means Logical. Sets whether or not to plot group means. Defaults to `FALSE`.
#' @param boxwidth Numeric. Sets the width of the bars. Defaults to 0.7.
#' @param whisker.width Numeric. Sets the width of the error bar whiskers. Defaults to 1.
#' @param mean.size Numeric. Sets the size of group mean points. Defaults to 1.
#' @param points Logical. Sets whether or not to plot individual data points. Defaults to `TRUE`.
#' Note that, even if set to `FALSE`, outliers will still be shown as individual data points, as is the norm with box plots.
#' @param jitterwidth Numeric. The horizontal dispersion of individual data points. Defaults to 1.
#' @param pointsize Numeric. Sets the size of the individual data points. Defaults to 1.
#' @param mean.type Character. Sets from arithmetic to geometric mean and SD. Defaults to "arithmetic".
#' @return A plot based on `ggplot2`.
#'
#' #' @import ggplot2
#' @import ggthemes
#' @import data.table
#' @importFrom rlang sym
#'
box_mean_plot <- function(d,x=NULL,y,color.group=NULL,x.axis=NULL,y.axis=NULL,legend.title=NULL,
                          x.order=NULL,group.order=NULL,x.first=NULL,group.first=NULL,
                          means=FALSE,boxwidth=0.7,whisker.width=1,mean.size=1,points=TRUE,jitterwidth=1,pointsize=1,
                          mean.type="arithmetic") {

  if(!is.data.frame(d)) {
    stop("Object supplied as d is not a data.frame")
  }

  if(!(y %in% names(d))) {
    stop(paste("Supplied",y,"field does not exist in d."))
  }

  if(!(class(d[[y]]) %in% c("numeric","integer"))) {
    stop(paste("Supplied",y,"is not a numeric field in d."))
  }

  if(is.null(x) & is.null(color.group)) {
    stop("Supplied x and color.group are both NULL. Please specify at least one of them.")
  }

  if(!is.null(x)) {
    if(!(x %in% names(d))) {
      stop(paste("Supplied",x,"field does not exist in d."))
    } else {
      if(!is.null(x.order)) {
        if(FALSE %in% (x.order %in% d[[x]])) {
          stop(paste("Categories in supplied x.order don't exist in",x,"field"))
        } else {
          d[[x]] <- factor(d[[x]],levels=x.order)
        }
      } else {
        d[[x]] <- factor(d[[x]])
      }
    }
  }

  if(!is.null(color.group)) {
    if(!(color.group %in% names(d))) {
      stop(paste("Supplied",color.group,"field does not exist in d."))
    } else {
      if(!is.null(group.order)) {
        if(FALSE %in% (group.order %in% d[[color.group]])) {
          stop(paste("Categories in supplied group.order don't exist in",color.group,"field"))
        } else {
          d[[color.group]] <- factor(d[[color.group]],levels=group.order)
        }
      } else {
        d[[color.group]] <- factor(d[[color.group]])
      }
    }
  }

  if(!is.null(x.first)) {
    if(!(x.first %in% d[[x]])) {
      stop(paste(x.first,"does not exist in",x,"field"))
    } else if(!is.null(x.order)) {
      warning("x.order was supplied. x.first will be ignored.")
    } else {
      d[[x]] <- relevel(d[[x]],x.first)
    }
  }

  if(!is.null(group.first)) {
    if(!(group.first %in% d[[color.group]])) {
      stop(paste(group.first,"does not exist in",color.group,"field"))
    } else if(!is.null(group.order)) {
      warning("group.order was supplied. group.first will be ignored.")
    } else {
      d[[color.group]] <- relevel(d[[color.group]],group.first)
    }
  }

  if(!(mean.type %in% c("arithmetic","geometric"))) {
    stop("Supplied mean.type is not 'arithmetic' or 'geometric'.")
  }

  show.outliers <- ifelse(points==TRUE,NA,1)

  if(is.null(x)) {
    mapping <- aes(x="constant",y=!!sym(y),fill=!!sym(color.group))

    box.position <- position_dodge(width=boxwidth)

    box.data <- geom_boxplot(position=box.position,
                             width=boxwidth,
                             colour="black",
                             outlier.shape=show.outliers,
                             outlier.size=1.5*pointsize)

    point.position <- position_jitterdodge(jitter.width=jitterwidth*0.5*boxwidth,dodge.width=boxwidth)

    aesthetics <- list(scale_fill_grey(limits=levels(d[[color.group]])),
                       theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
                       labs(fill=ifelse(is.null(legend.title),color.group,legend.title)))

  } else if(is.null(color.group)) {
    mapping <- aes(x=!!sym(x),y=!!sym(y))

    box.position <- position_identity()

    box.data <- geom_boxplot(position=box.position,
                             width=boxwidth,
                             colour="black",fill="grey20",
                             outlier.shape=show.outliers,
                             outlier.size=1.5*pointsize)

    point.position <- position_jitter(width=jitterwidth*0.5*boxwidth)

    aesthetics <- xlab(ifelse(is.null(x.axis),x,x.axis))

  } else {
    mapping <- aes(x=!!sym(x),y=!!sym(y),fill=!!sym(color.group))

    box.position <- position_dodge(width=boxwidth)

    box.data <- geom_boxplot(position=box.position,
                             width=boxwidth,
                             colour="black",
                             outlier.shape=show.outliers,
                             outlier.size=1.5*pointsize)

    point.position <- position_jitterdodge(jitter.width=jitterwidth*0.5*boxwidth,dodge.width=boxwidth)

    aesthetics <- list(scale_fill_grey(limits=levels(d[[color.group]])),
                       xlab(ifelse(is.null(x.axis),x,x.axis)),
                       labs(fill=ifelse(is.null(legend.title),color.group,legend.title)))
  }


  if(points==TRUE) {
    point.data <- geom_point(size=1.5*pointsize,
                             position=point.position,
                             show.legend=FALSE,
                             shape=1)
  } else {
    point.data <- NULL
  }



  #-----------------------------CALCULATING MEANS AND MAXIMUM PLOTTED VALUES-------------------------------

  if(means==TRUE) {
    if(mean.type=="arithmetic") {
      # Arithmetic means
      mean.data <- stat_summary(fun=mean,
                                geom="point",
                                position=box.position,
                                shape=4,stroke=1.5*mean.size,colour="black")

    } else if(mean.type=="geometric") {
      # Geometric means
      mean.data <- stat_summary(fun=function(x) {log(x) |> mean(na.rm=TRUE) |> exp()},
                                geom="point",
                                position=box.position,
                                shape=4,stroke=1.5*mean.size,colour="black")

    } else {
      stop("Supplied mean.type is not 'arithmetic' or 'geometric'.")
    }
  } else {
    mean.data <- NULL
  }



  #-----------------------------------------START PLOTTING-----------------------------------------

  plot <- ggplot(data=d,mapping=mapping) +

    #use a theme for formatting
    theme_classic() +
    theme(axis.ticks.x=element_blank()) +

    #Stick the bars to the x axis and expand the y axis to +5% of the maximum data
    scale_y_continuous(expand=expansion(mult=c(0,0.075)),limits=c(0,NA)) +

    #add the axis titles
    ylab(ifelse(is.null(y.axis),y,y.axis)) +

    stat_boxplot(geom="errorbar",
                 width=whisker.width*boxwidth*0.2,
                 position=box.position) +

    box.data +

    point.data +

    mean.data +

    aesthetics

  return(plot)

}
