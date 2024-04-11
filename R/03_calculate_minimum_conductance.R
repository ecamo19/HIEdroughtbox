
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
#' `read_hie_droughtbox_leaf_branch_areas()`
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
                                 leaf_and_branch_area_data,
                                 calculate_gres = TRUE
                                 ){

    # Validate input parameters ------------------------------------------------

    # Stop if droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Stop if droughtbox_data is not a data frame
    base::stopifnot("leaf_and_branch_area_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(leaf_and_branch_area_data))

    # Assert date column in droughtbox_data
    checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in droughtbox_data" =  c("strain_avg_1_microstrain_avg",
                                                              "strain_avg_2_microstrain_avg",
                                                              "strain_avg_3_microstrain_avg",
                                                              "strain_avg_4_microstrain_avg",

                                                              "set_point_t_avg_avg",
                                                              "vpd_avg_kpa_avg",
                                                              "date_time"
                                                              ) %in% base::colnames(droughtbox_data))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in the leaf_and_branch_area_data" =  c("leaf_area_cm2",
                                                                            "surface_branch_area_cm2",
                                                                            "strain_number",
                                                                            "set_temperature",
                                                                            "sample_id"
                                                                            ) %in% base::colnames(leaf_and_branch_area_data))

    # Calculate leaf residual conductance --------------------------------------

    # Transform the data into the right format
    droughtbox_data %>%

        # Select only the necessary variables calculating
        dplyr::select(date_time,

                      # Get set temperature inside the box
                      set_point_t_avg_avg,

                      # Get weight loss variables
                      strain_avg_1_microstrain_avg,
                      strain_avg_2_microstrain_avg,
                      strain_avg_3_microstrain_avg,
                      strain_avg_4_microstrain_avg)  %>%

        # Reshape data into a long format
        tidyr::pivot_longer(!c(date_time, set_point_t_avg_avg),

                            # Create new columns
                            names_to = "strains",
                            values_to = "strain_weight") %>%

        # Transform strain_number to factor type to keep consistent
        dplyr::mutate(strains = base::factor(strains)) %>%

        # Create new column with the new names for each strain
        dplyr::mutate(strain_number = dplyr::case_when(strains == "strain_avg_1_microstrain_avg"  ~ "strain_1",
                                                       strains == "strain_avg_2_microstrain_avg"  ~ "strain_2",
                                                       strains == "strain_avg_3_microstrain_avg"  ~ "strain_3",
                                                       strains == "strain_avg_4_microstrain_avg"  ~ "strain_4",
                                                       TRUE ~ strains),
                      # Remove unused col
                      .keep = "unused") %>%

        # Rename column
        dplyr::rename(., set_temperature = set_point_t_avg_avg) %>%

        ## Get the slope between the measured weight loss and time -------------

        # Create a 'mini' dataframes by strain_number, set_temperature
        tidyr::nest(data = -c(strain_number, set_temperature)) %>%

        # Calculate the slope
        dplyr::mutate(slope = purrr::map(data, ~coef(lm(strain_weight ~ date_time, data = .x))[["date_time"]])) %>%

        # Remove 'mini' dataframes
        dplyr::select(-data) %>%

        # Show slope data
        tidyr::unnest(cols = slope)

    ### Raise warning if Positive slope found

    ## Estimate transpiration --------------------------------------------------
    #e <- -(slope/(total_leaf_area_m2 + branch_surface_area_m2))

    ## Estimate leaf residual conductance --------------------------------------

}
