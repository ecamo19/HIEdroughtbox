
#' residual_conductance
#'
#' @description
#' This function calculates residual leaf conductance (gmin) or residual
#' branch conductance (gres) following equation 2 found the research paper
#' titled 'The DroughtBox: A new tool for phenotyping residual branch
#' conductance and its temperature dependence during drought' by Billon and
#' colleges.
#'
#'s @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data()`
#'
#' @param leaf_and_stem_area Dataframe loaded with the function
#' `read_hie_droughtbox_leaf_branch_areas`
#'
#'
#' containing leaf area and/or branch
#' surface area. This
#'
#' @param calculate_gres Boolean indicating if residual branch conductance (TRUE)
#' or leaf residual conductance (FALSE) should be calculated
#'
#' @importFrom magrittr %>%
#'
#' @return
#'
#' @examples
#'
#' @export
residual_conductance <- function(droughtbox_data,
                                 leaf_and_stem_area,
                                 calculate_gres = TRUE
                                 ){

    # Validate input parameters ------------------------------------------------

    # Stop of droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Assert date column in droughtbox_data
    checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure that the data is in the dataframe
    base::stopifnot("Missing columns in the dataframe" =  c("strain_avg_1_microstrain_avg",
                                                            "strain_avg_2_microstrain_avg",
                                                            "strain_avg_3_microstrain_avg",
                                                            "strain_avg_4_microstrain_avg",

                                                            "vpd_avg_kpa_avg",
                                                            "date_time"
    ) %in% base::colnames(droughtbox_data))


    # Calculate leaf residual conductance --------------------------------------

    ## Get the slope between the measured weight loss and time -----------------

    ## Estimate transpiration --------------------------------------------------
    #e <- -(slope/(total_leaf_area_m2 + branch_surface_area_m2))

    ## Estimate leaf residual conductance --------------------------------------

}
