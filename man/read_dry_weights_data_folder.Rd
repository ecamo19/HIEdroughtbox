% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_droughtbox_utils.R
\name{read_dry_weights_data_folder}
\alias{read_dry_weights_data_folder}
\title{read_dry_weights_data_folder}
\usage{
read_dry_weights_data_folder(path_droughtbox_data_folder)
}
\arguments{
\item{path_droughtbox_data_folder}{containing}

\item{paper_bag_group}{Integer (either 1 or 2) indicating the paper bag group where samples were dried}
}
\value{
A dataframe of with each sample dry
}
\description{
This function reads the raw .dat file downloaded from the droughtbox located
at the Hawkesbury Institute for the Environment.
}
\examples{
\dontrun{read_dry_weights_data_folder()}

}
