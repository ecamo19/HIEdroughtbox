% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_droughtbox_plots.R
\name{plot_strains_weights}
\alias{plot_strains_weights}
\title{plot_strains_weights}
\usage{
plot_strains_weights(
  droughtbox_data,
  show_strain = "all",
  show_tare_group = TRUE,
  show_smooth_lines = TRUE,
  time_breaks = "10 min"
)
}
\arguments{
\item{droughtbox_data}{Dataframe loaded with the function.
`read_hie_droughtbox_data`}

\item{show_strain}{String (i.e. "strain_1") or vector of strings
(c("strain_2", "strain_3")) indicating which strain to plot.
Default is "all".}

\item{show_tare_group}{Boolean (TRUE/FALSE) indicating if data should
be shown as points or as labels with the tare_count to which they
belong to.}

\item{time_breaks}{String indicating the resolution at which the x-axis
should show the tick marks. Choose one of "sec", "min", "hour", "day",
"week", "month", "year".}
}
\value{
A ggplot2 object with the weight (grams) measured by each strain (4 in
total) inside the Droughtbox.
}
\description{
This function displays the raw weights (grams) measured inside the
Droughtbox.
}
\examples{
\dontrun{
path_to_droughtbox_data <- system.file("extdata",
                            "acacia_aneura_25c.dat",
                            package = "HIEdroughtbox")

droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)

plot_strains_weights(droughtbox_data)
}
}
