#' Plot export in multiple formats
#'
#' @description
#' Exports plots based on `ggplot2` in vector (PDF, SVG), raster (PNG) and R data format (Rds).
#'
#' @details
#' PNG and PDF files are exported using the `ggplot2`-based `ggsave` function.
#' Exported PNG images are anti-aliased using the Cairo graphics library.
#' Exported SVG graphics are drawn using the R-native SVG graphics device. This is because SVG graphics exported by `ggsave`
#' are not rendered properly by the graphic design software Affinity Designer.
#' The downside of exporting SVG with the native graphics device is that the text is drawn as paths instead of text,
#' so it cannot be easily modified as text in graphic design software (e.g. set as bold, italics, etc).
#'
#' @param plot Plot to be exported. Needs to be based on `ggplot2` (`datavis`-created plots comply with this).
#' @param filepath Character. The filepath for the exported plot, relative to the working directory.
#' Do not add file extension, appropriate extension will be added to each exported file.
#' @param height Numeric. The height of the exported plot in the desired length unit.
#' @param width Numeric. The width of the exported plot in the desired length unit.
#' @param unit Character. The unit to be used as `height` and `width`.
#' "in" for inches, "cm" for centimeters and "mm" for millimeters. Defaults to "in".
#' @param dpi Integer. The resolution of the exported PNG image in dots per inch. Defaults to 600.
#'
#' @return Exports plot without returning a value.
#'
#' @import Cairo
#'
#' @export
#'
plot_save <- function(plot,filepath,height,width,unit="in",dpi=600) {

  if(unit!="in" & unit!="cm" & unit!="mm") {# Test if proper length unit was given
    stop('Error! Please specify the length unit to be used as "in","cm" or "mm"')
  }

  # Save to pdf
  ggplot2::ggsave(filename=paste0(filepath,"_",width,"x",height,unit,".pdf"),
                  plot=plot,
                  height=height,width=width,units=unit)

  # Save to png
  ggplot2::ggsave(filename=paste0(filepath,"_",width,"x",height,unit,".png"),
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
  filedirectory<-substr(filedirectory,2,stringr::str_length(filedirectory))
  if(filedirectory=="") {
    filedirectory<-"the working directory"
  }
  message(paste0('Plot "',filename,'" saved as pdf, png, svg and Rds in ',filedirectory))

}
