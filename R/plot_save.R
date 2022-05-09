#' Plot export in multiple formats
#'
#' Exports plots based on `ggplot2` in vector (PDF, SVG), raster (PNG) and R data format (Rds).
#'
#' @param plot Plot to be exported. Needs to have been produced using `ggplot2` (functions described in this guide comply with this).
#' @param filepath Character. The directory and file name for the exported plot.
#' Do not add file extension, the appropriate extension will be added to each exported file.
#' @param height Numeric. The height of the exported plot in the desired length unit.
#' @param width Numeric. The width of the exported plot in the desired length unit.
#' @param unit Character. The unit to be used as `height` and `width`.
#' "in" for inches, "cm" for centimeters and "mm" for millimeters. Defaults to "in".
#' @param dpi Integer. The resolution of the exported PNG image in dots per inch. Defaults to 600.
#' @return Exports plot without returning a value.

plot_save <- function(plot,filepath,height,width,unit="in",dpi=600) {

  if(unit!="in" & unit!="cm" & unit!="mm") {# Test if proper length unit was given
    stop('Error! Please specify the length unit to be used as "in","cm" or "mm"')
  }
  suppressMessages(library(ggplot2))
  suppressMessages(library(measurements))
  suppressMessages(library(Cairo))
  suppressMessages(library(stringr))


  # Save to pdf
  ggsave(filename=paste0(filepath,"_",width,"x",height,unit,".pdf"),
         plot=plot,
         height=height,width=width,units=unit)

  # Save to png
  ggsave(filename=paste0(filepath,"_",width,"x",height,unit,".png"),
         plot=plot,
         height=height,width=width,units=unit,dpi=dpi,
         type="cairo")

  # Save to svg
  # It is done with the svg graphics device, because ggplots saved with ggsave/svglite cannot be read properly by Affinity Designer
  # Initial conversion of units, because the svg graphics device recognizes only inches
  if(unit!="in") {
    height.svg<-measurements::conv_unit(height,from=unit,to="inch")
    width.svg<-measurements::conv_unit(width,from=unit,to="inch")
  } else {
    height.svg<-height
    width.svg<-width
  }

  svg(filename=paste0(filepath,"_",width,"x",height,unit,".svg"),
      height=height.svg,width=width.svg)
  print(plot)
  dev.off()

  # Save to Rds
  saveRDS(object=plot,file=paste0(filepath,".Rds"))

  # Print saving information as message
  split.file<-strsplit(filepath,"/")
  filename<-split.file[[1]][length(split.file[[1]])]
  filedirectory<-""
  for(i in 1:length(split.file[[1]])-1) {
    filedirectory<-paste(filedirectory,split.file[[1]][i],sep="/")
  }
  filedirectory<-substr(filedirectory,2,str_length(filedirectory))
  if(filedirectory=="") {
    filedirectory<-"the working directory"
  }
  message(paste0('Plot "',filename,'" saved as pdf, png, svg and Rds in ',filedirectory))

}
