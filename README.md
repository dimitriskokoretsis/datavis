
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datavis

## R package for easy data visualization

<!-- badges: start -->
<!-- badges: end -->

The goal of `datavis` is to simplify the creation, annotation and export
of bar plots and box plots of publication-grade quality. It is meant for
academic researchers of any programming experience - newcomers and
programming experts alike.

To a large extent, `datavis` wraps around the well-known [`ggplot2`
package](https://ggplot2.tidyverse.org/), but specializes in *very*
specific plots (dodged bar and box plots based on sample distributions),
aiming to increase efficiency in handling these plots.

This is a short summary of `datavis`’ functionality. For a comprehensive
guide aimed towards programming beginners, check the handbook [Data
visualization with the ‘datavis’ R
package](https://github.com/dimitriskokoretsis/datavis/raw/master/guide/Data-visualization-with-the-datavis-R-package.pdf).
For a full description of the `datavis` functions, check their help
documentation in [pdf
form](https://github.com/dimitriskokoretsis/datavis/raw/master/guide/datavis_0.1.0.pdf)
or by browsing the package after installation.

## Why to use datavis

Using the `datavis` functions, you can easily perform the following
tasks with very few lines of code:

-   Create bar/box plots with summary statistics:

    -   arithmetic or geometric mean

    -   plus/minus corresponding standard deviation, or custom,
        user-calculated error measure

    -   optionally, individual data points

-   Customize plots aesthetically.

-   Annotate plots with custom text above bars/boxes.

-   Export plots in various formats for any purpose.

## Installation

You can install `datavis` from [GitHub](https://github.com/) by
executing the following lines in the R console:

``` r
install.packages("devtools")
devtools::install_github("dimitriskokoretsis/datavis")
```

## Example

The following example demonstrates the use of `datavis` functions to
create, annotate and export a bar plot.

### Data import and plot creation

``` r
# Data import using the fread function of the data.table package
demo.data.1 <- data.table::fread("guide/demo_data/demo_data_1.csv")
knitr::kable(demo.data.1)
```

| factor.1 | factor.2 | value |
|:---------|:---------|------:|
| A        | C        |  9.00 |
| A        | C        | 10.26 |
| A        | C        |  9.84 |
| A        | C        | 11.77 |
| A        | C        | 10.23 |
| A        | D        | 20.64 |
| A        | D        | 18.84 |
| A        | D        | 21.43 |
| A        | D        | 18.35 |
| A        | D        | 19.28 |
| B        | C        | 15.18 |
| B        | C        | 15.19 |
| B        | C        | 14.60 |
| B        | C        | 16.48 |
| B        | C        | 15.25 |
| B        | D        | 11.94 |
| B        | D        | 11.22 |
| B        | D        | 13.02 |
| B        | D        | 10.17 |
| B        | D        | 16.62 |

``` r
# Loading of datavis package
library(datavis)

# Creation of bar plot with the bar_point_plot function of datavis
# See function's help documentation for more information
plot.1 <- demo.data.1 |>
  bar_point_plot(x="factor.1", # X axis grouping based on "factor.1" field
                 y="value", # Y axis value is "value" field
                 color.group="factor.2", # Color grouping based on "factor.2" field
                 x.axis="Factor 1", # Give a better title to x axis
                 y.axis="Value", # Give a better title to y axis
                 legend.title="Factor 2", # Give a better title to the legend
                 jitterwidth=0.7) # Adjust horizontal jitter of individual data points

plot.1
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Import of statistics data and plot annotation

``` r
# Import of statistics data from Tukey's honest significant difference (HSD) test
demo.data.1.TukeyHSD <- data.table::fread("guide/demo_data/demo_data_1_TukeyHSD.csv")
knitr::kable(demo.data.1.TukeyHSD)
```

| HSDgroups | factor.1 | factor.2 |
|:----------|:---------|:---------|
| a         | A        | D        |
| b         | B        | C        |
| bc        | B        | D        |
| c         | A        | C        |

``` r
# Annotation of original plot with the plot_stats function of datavis
# See function's help documentation for more information
plot.1.TukeyHSD <- plot.1 |>
  plot_stats(d=demo.data.1.TukeyHSD, # The data.frame containing the labels to be plotted.
             labels="HSDgroups", # The name of the labels column in the supplied data.frame.
             position="dodge") # Positioning of labels in the X dimension.

plot.1.TukeyHSD
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### Plot export

``` r
# Export of plot in PDF, SVG, PNG and Rds formats with the plot_save function of datavis
# See function's help documentation for more information
plot.1.TukeyHSD |>
  plot_save(filepath="guide/demo_plots/plot_1_TukeyHSD", # Path to export files
            height=4,width=5) # Dimensions in inches
#> Plot "plot_1_TukeyHSD" saved as pdf, png, svg and Rds in /guide/demo_plots
```
