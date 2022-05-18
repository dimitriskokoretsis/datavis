#' Bar plots of grouped data with summary statistics
#'
#' @description
#' Create bar plots of grouped data means plus/minus standard deviations (or custom, manually calculated error measure).
#'
#' @details
#' The calculations for mean and standard deviation are performed automatically. To show other error measure,
#' calculate it manually and include it as two fields in the supplied `data.frame`
#' (field names are supplied as `error.lower` and `error.upper` arguments).
#' To change between arithmetic and geometric mean plus/minus standard deviation, set `mean.type`
#' argument to "arithmetic" or "geometric".
#'
#' The order of groups shown in the x axis and color groups defaults to alphabetical.
#' To change it, supply `x.order` or `group.order` arguments.
#' These should be character vectors with the desired order of each factor.
#' Do not include categories that don't exist in the supplied `data.frame`.
#' To bring just one category first, supply `x.first` or `group.first` arguments.
#' If `x.order` is supplied, `x.first` will be ignored. The same holds for `group.order` and `group.first`.
#'
#' Adjust other supplied arguments to customize the plot aesthetically.
#'
#' @param d `data.frame` with data to be plotted.
#' @param x Character. The name of the column to be used for the x axis (categorical data).
#' Defaults to `NULL`, but if `NULL`, `color.group` needs to be supplied.
#' @param y Character. The name of the column to be used for the y axis (numeric data).
#' @param color.group Character. The name of the column to be used for color grouping (categorical data).
#' Defaults to `NULL`, but if `NULL`, `x` needs to be supplied.
#' @param x.axis Character. The title of the x axis. If `NULL`, x axis title is given the value of `x`.
#' Defaults to `NULL`.
#' @param y.axis Character. The title of the y axis. If `NULL`, y axis title is given the value of `y`.
#' Defaults to `NULL`.
#' @param legend.title Character. The title of the legend. If `NULL`, legend title is given the value of `color.group`.
#' Defaults to `NULL`.
#' @param x.order Character vector of length equal to the number of x categories.
#' Determines the order of categories in the x axis, respectively. Defaults to alphabetical order.
#' @param group.order Character vector of length equal to the number of color grouping categories.
#' Determines the order of color groups. Defaults to alphabetical order.
#' @param x.first Character. Places a specific x axis category first. Ignored if `x.order` is supplied.
#' Defaults to `NULL`.
#' @param group.first Character. Places a specific color group category first. Ignored if `group.order` is supplied.
#' Defaults to `NULL`.
#' @param points Logical. Sets whether or not to plot individual data points. Defaults to `TRUE`.
#' @param barwidth Numeric. Sets the width of the bars. Defaults to 0.7.
#' @param jitterwidth Numeric. The horizontal dispersion of individual data points. Defaults to 1.
#' @param pointsize Numeric. Sets the size of the individual data points. Defaults to 1.
#' @param whisker.width Numeric. Sets the width of the error bar whiskers. Defaults to 1.
#' @param mean.type Character, either "arithmetic" or "geometric". Sets which type of mean and standard deviation to plot. Defaults to "arithmetic".
#' @param error.lower Character, name of a column in the `d` `data.frame`.
#' Plot custom lower error bar, calculated by the user and included in the `d` data frame as a column. Defaults to `NULL`, which plots `mean - SD`.
#' @param error.upper Character, name of a column in the `d` `data.frame`.
#' Plot custom upper error bar, calculated by the user and included in the `d` data frame as a column. Defaults to `NULL`, which plots `mean + SD`.
#'
#' @return A plot based on `ggplot2`.
#'
#' @import ggplot2
#' @import ggthemes
#'
#' @export
#'
bar_point_plot <- function(d,x=NULL,y,color.group=NULL,x.axis=NULL,y.axis=NULL,legend.title=NULL,
                           x.order=NULL,group.order=NULL,x.first=NULL,group.first=NULL,
                           points=TRUE,barwidth=0.7,jitterwidth=1,pointsize=1,whisker.width=1,
                           mean.type="arithmetic",error.lower=NULL,error.upper=NULL) {

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

  if(!is.null(error.lower)) {
    if(!(error.lower %in% names(d))) {
      stop(paste("Supplied",error.lower,"field does not exist in d."))
    }
  }

  if(!is.null(error.upper)) {
    if(!(error.upper %in% names(d))) {
      stop(paste("Supplied",error.upper,"field does not exist in d."))
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

  if(mean.type=="arithmetic") {
    mean.function <- function(x) {mean(x,na.rm=TRUE)}
    lower.error.function <- function(x) {mean(x) - sd(x)}
    upper.error.function <- function(x) {mean(x) + sd(x)}
  } else if(mean.type=="geometric") {
    mean.function <- function(x) {log(x) |> mean(na.rm=TRUE) |> exp()}
    lower.error.function <- function(x) {((log(x) |> mean(na.rm=TRUE)) - (log(x) |> sd(na.rm=TRUE))) |> exp()}
    upper.error.function <- function(x) {((log(x) |> mean(na.rm=TRUE)) + (log(x) |> sd(na.rm=TRUE))) |> exp()}
  } else {
    stop("Supplied mean.type is not 'arithmetic' or 'geometric'.")
  }

  if(is.null(x)) {
    mapping <- aes(x="constant",y=!!rlang::sym(y),fill=!!rlang::sym(color.group))

    bar.position <- position_dodge(width=barwidth)

    bar.data <- stat_summary(fun=mean.function,
                             geom="bar",
                             position=bar.position,
                             width=barwidth,colour="black")

    point.position <- position_jitterdodge(jitter.width=jitterwidth*0.5*barwidth,dodge.width=barwidth)

    aesthetics <- list(scale_fill_grey(limits=levels(d[[color.group]])),
                       theme(axis.title.x=element_blank(),axis.text.x=element_blank()),
                       labs(fill=ifelse(is.null(legend.title),color.group,legend.title)))

  } else if(is.null(color.group)) {
    mapping <- aes(x=!!rlang::sym(x),y=!!rlang::sym(y))

    bar.position <- position_identity()

    bar.data <- stat_summary(fun=mean.function,
                             geom="bar",
                             position=bar.position,
                             width=barwidth,colour="black",fill="grey20")

    point.position <- position_jitter(width=jitterwidth*0.5*barwidth)

    aesthetics <- xlab(ifelse(is.null(x.axis),x,x.axis))

  } else {
    mapping <- aes(x=!!rlang::sym(x),y=!!rlang::sym(y),fill=!!rlang::sym(color.group))

    bar.position <- position_dodge(width=barwidth)

    bar.data <- stat_summary(fun=mean.function,
                             geom="bar",
                             position=bar.position,
                             width=barwidth,colour="black")

    point.position <- position_jitterdodge(jitter.width=jitterwidth*0.5*barwidth,dodge.width=barwidth)

    aesthetics <- list(scale_fill_grey(limits=levels(d[[color.group]])),
                       xlab(ifelse(is.null(x.axis),x,x.axis)),
                       labs(fill=ifelse(is.null(legend.title),color.group,legend.title)))
  }



  if(points==TRUE) {
    point.data <- geom_point(size=1.5*pointsize,
                             position=point.position,
                             show.legend=FALSE)
  } else {
    point.data <- NULL
  }

  if((is.null(error.lower) | is.null(error.upper))) {

    error.data <- stat_summary(fun.min=lower.error.function,
                               fun.max=upper.error.function,
                               geom="errorbar",
                               position=bar.position,
                               colour="black",width=whisker.width*barwidth*0.2)
  } else {
    error.data <- geom_errorbar(mapping=aes(ymin=!!sym(error.lower),ymax=!!sym(error.upper)),
                                position=bar.position,
                                colour="black",width=whisker.width*barwidth*0.2)
  }

  #-----------------------------------------START PLOTTING-----------------------------------------
  plot <- ggplot(data=d,mapping=mapping) +
    #use a theme for formatting
    theme_classic() +
    theme(axis.ticks.x=element_blank()) +

    #Stick the bars to the x axis and expand the y axis to +7.5% of the maximum data
    scale_y_continuous(expand=expansion(mult=c(0,0.075))) +

    #add the axis titles
    ylab(ifelse(is.null(y.axis),y,y.axis)) +

    bar.data +

    error.data +

    point.data +

    aesthetics

  return(plot) #return the resulted plot
}
