
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

    # Determine the relationship between VPD and time --------------------------
    vpd_data <-
        droughtbox_data %>%

        # Select only the necessary variables
        dplyr::select(time,
                      vpd_avg_kpa_avg,
                      set_point_t_avg_avg) %>%

        # Rename variables
        dplyr::mutate(set_temperature = as.integer(set_point_t_avg_avg),

                      # Transform time to seconds
                      time_seconds = (time - dplyr::first(time)),
                      .keep = "unused")

    # If there is a high correlation then calculate the slope
    if(cor(vpd_data$vpd_avg_kpa_avg , as.numeric(vpd_data$time_seconds)) >= 0.60){

        vpd_parameters <-
            vpd_data %>%
                dplyr::group_by(set_temperature) %>%
                tidyr::nest(data = -c(set_temperature)) %>%

                # Get the slope
                dplyr::mutate(slope_vpd_per_second = purrr::map(data, ~coef(lm(vpd_avg_kpa_avg ~ time_seconds,
                                                                         data = .x))[["time_seconds"]])) %>%
                dplyr::select(-data) %>%
                tidyr::unnest(cols = slope_vpd_time) %>%
                dplyr::ungroup()

    # Else calculate the mean
    } else if(cor(vpd_data$vpd_avg_kpa_avg , as.numeric(vpd_data$time_seconds)) < 0.60 &
              cor(vpd_data$vpd_avg_kpa_avg , as.numeric(vpd_data$time_seconds)) >= 0) {

        vpd_parameters <-
            vpd_data %>%
                dplyr::group_by(set_temperature) %>%

                # Get the mean
                dplyr::summarise(mean_vpd = mean(vpd_avg_kpa_avg)) %>%
                dplyr::ungroup()

    } else{
        stop("Failed in calculating VPD parameter in residual_conductance function")
    }

    # Prepare data  ------------------------------------------------------------

    # Transform the data into the right format
    droughtbox_data %>%

        # Select only the necessary variables calculating
        dplyr::select(time,
                      set_point_t_avg_avg,

                      # Get weight loss variables
                      strain_avg_1_microstrain_avg,
                      strain_avg_2_microstrain_avg,
                      strain_avg_3_microstrain_avg,
                      strain_avg_4_microstrain_avg)  %>%

        # Reshape data into a long format
        tidyr::pivot_longer(!c(time, set_point_t_avg_avg),

                            # Create new columns
                            names_to = "strains",
                            values_to = "strain_weight") %>%

        # Create new column with the new names for each strain
        dplyr::mutate(strain_number = dplyr::case_when(strains == "strain_avg_1_microstrain_avg"  ~ "1",
                                                       strains == "strain_avg_2_microstrain_avg"  ~ "2",
                                                       strains == "strain_avg_3_microstrain_avg"  ~ "3",
                                                       strains == "strain_avg_4_microstrain_avg"  ~ "4",
                                                       TRUE ~ strains),
                      # Remove unused col
                      .keep = "unused") %>%

        # Step done for transforming time to seconds
        dplyr::group_by(strain_number, set_point_t_avg_avg) %>%

        # Transform columns
        dplyr::mutate(set_temperature = as.integer(set_point_t_avg_avg),

                      # Get time in seconds
                      time_seconds = (time - dplyr::first(time)),

                      .keep = "unused") %>%

        dplyr::mutate(strain_number = as.integer(strain_number)) %>%

        # Determine the relationship between weight loss and time --------------

        # Create a nested dataframes by strain_number, set_temperature
        tidyr::nest(data = -c(strain_number, set_temperature)) %>%

        # Create column with the slopes by strain_number, set_temperature
        dplyr::mutate(slope_grams_per_second = purrr::map(data,

                                                           # Calculate the slope
                                                     ~coef(lm(strain_weight ~ time_seconds,
                                                              data = .x))[["time_seconds"]])) %>%

        # Remove nested dataframes
        dplyr::select(-data) %>%

        # Unnest slope data
        tidyr::unnest(cols = slope_grams_per_second) %>%

        # Print message if positive slope found
        {dplyr::if_else(.$slope_grams_per_second > 0,
                        print("Positive slope between weight loss and time found.Check your data"),
                        "Negative slope. This is OK"); .} %>%

        # Estimate transpiration -----------------------------------------------

        ## Merge leaf and branch areas data with slope data --------------------
        dplyr::full_join(., areas, by = c("strain_number", "set_temperature")) %>%

        # Without this the code won't run
        dplyr::ungroup() %>%

        # Print message if surface_branch_area_cm2 is found in the data or not
        {if("surface_branch_area_cm2" %in% names(.)) print("Transpiration for gres calculated")else print("Transpiration for gmin calculated"); .} %>%

        # Calculate transpiration
        dplyr::mutate(transpiration_grams_per_sec_cm2 =
                            if("surface_branch_area_cm2" %in% names(.))

                                # Transpiration for gres
                                -(.$slope_grams_per_second/(.$leaf_area_cm2 + .$surface_branch_area_cm2))

                            # Transpiration for gmin
                            else -(.$slope_grams_per_second/(.$leaf_area_cm2))) %>%

        # Remove variables. Done in this way because surface_branch_area_cm2 may or
        # may not present
        dplyr::select(-dplyr::any_of(c("slope_grams_per_second","leaf_area_cm2",
                                       "surface_branch_area_cm2"))) %>%

        # Arrange dataset
        dplyr::select(species_name, sample_id, dplyr::everything())

    # Estimate residual conductance --------------------------------------------
    # gres = (transpiration_grams_per_sec_cm2 / vpd_parameter)*101.6Kpa
    #
    # If vpd_parameter is a slope then gres units are:
    #
    # grams*sec*kpa
    # Kpa*sec*cm2
    #
    #


    return(tibble::as_tibble(.))

}
