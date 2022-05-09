.get_top_y <- function(plot,position,x.name,fill.name) {
  # Get plotted data from ggplot_build(plot)$data
  # Get original data from plot$data (only for factor level names)

  suppressMessages(library(data.table))
  suppressMessages(library(ggplot2))

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
    top.y <- CJ(x=x.levels,fill=fill.levels,sorted=FALSE)
    top.y[,`:=`(x=factor(x,levels=x.levels),fill=factor(fill,levels=fill.levels))]
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
