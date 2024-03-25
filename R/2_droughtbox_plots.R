#' plot_droughtbox_climatic_controls
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#'
#' @importFrom magrittr %>%
#'
#' @param cowplot Boolean indicating if plots should be arranged in one single
#' figure with 2 columns and 2 rows (TRUE) or if should be arranged individually
#'
#' @return A ggplot2 object with a total of 4 figures
#' @export
#'
#' @examples
#' droughtbox_data <- read_hie_droughtbox_data("data/acacia_aneura_25c.dat")
#' plot_droughtbox_climatic_controls(droughtbox_data)

plot_droughtbox_climatic_controls <- function(droughtbox_data, cowplot = TRUE){

    # Validate input dataset ---------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" =  "data.frame" %in% base::class(data))
    base::stopifnot("Missing columns in the dataframe" =  c(
                                                            "set_point_t_avg_avg",
                                                            "set_point_vpd_avg_avg",
                                                            "set_point_abs_h_avg_avg",
                                                            "set_point_rh_avg_avg",

                                                            "air_tc_avg_deg_c_avg",
                                                            "rh_avg_percent_avg",
                                                            "tc_avg_deg_c_avg",
                                                            "vpd_avg_kpa_avg",
                                                            "abs_h_avg_g_m3_avg"

                                                            ) %in% base::colnames(data))




}
