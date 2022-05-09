.get_dodge_width <- function(plot) {

  if(!is.null(plot$layers[[1]]$position$width)) {
    dodge.width <- plot$layers[[1]]$position$width
  } else {
    dodge.width <- plot$layers[[1]]$position$dodge.width
  }

  return(dodge.width)
}
