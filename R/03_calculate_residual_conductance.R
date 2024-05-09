#' calculate_residual_conductance
#'
#' @description
#' This function calculates residual leaf conductance (gmin) or residual
#' branch conductance (gres) following equation 2 found the research paper
#' titled 'The DroughtBox: A new tool for phenotyping residual branch
#' conductance and its temperature dependence during drought' by Billon and
#' colleagues.
#'
#'s @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data()`.
#'
#' @param leaf_and_branch_area_data Dataframe loaded with the function
#' `read_hie_droughtbox_leaf_branch_areas()`.
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe with the species_name, sample_id, strain_number,
#' set_temperature, transpiration_grams_per_sec_cm2 and
#' median_vpd residual_conductance as columns.
#'
#' @examples
#' path_droughtbox_leaf_branch_areas <- system.file("extdata",
#'                                                 "acacia_aneura_leaf_branch_areas.xlsx",
#'                                                 package = "HIEdroughtbox")
#'
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#' species_areas <- read_hie_droughtbox_leaf_branch_areas(path_droughtbox_leaf_branch_areas)
#'
#' calculate_residual_conductance(droughtbox_data = droughtbox_data,
#'                                leaf_and_branch_area_data = species_areas)
#'
#' @export
calculate_residual_conductance <- function(droughtbox_data,
                                           leaf_and_branch_area_data
                                 ){

    # Atmospheric pressure in the droughtbox constant
    atmospheric_pressure_constant = 101.6

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

    # Get VPD parameter --------------------------------------------------------
    # This parameter will be later used in estimating gres.

    vpd_parameter <-
        droughtbox_data %>%

        # Select only the necessary variables
        dplyr::select(time,
                      vpd_avg_kpa_avg,
                      set_point_t_avg_avg) %>%

        # Rename variables
        dplyr::mutate(set_temperature = as.integer(set_point_t_avg_avg),

                      # Transform time to seconds
                      time_seconds = (time - dplyr::first(time)),
                      .keep = "unused") %>%

        # Group by temperature
        dplyr::group_by(set_temperature) %>%

        # Print message
        {print("Make sure VPD conditions were constant"); .} %>%

        # Get the median
        dplyr::summarise(median_vpd = stats::median(vpd_avg_kpa_avg))

    # Prepare data  ------------------------------------------------------------

    residual_conductance_df <-

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

            # Determine the relationship between weight loss and time ----------

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
                        print("Positive slope between weight loss and time found. Check your data"),
                        "Negative slope. This is OK"); .} %>%

        # Estimate transpiration -----------------------------------------------

        ## Merge leaf and branch areas data with slope data --------------------
        dplyr::full_join(., leaf_and_branch_area_data,
                         by = c("strain_number", "set_temperature")) %>%

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
        dplyr::select(species_name, sample_id, dplyr::everything()) %>%


        # Add VPD parameter in the dataset
        dplyr::full_join(., vpd_parameter, by = c("set_temperature")) %>%

        # Estimate residual conductance ----------------------------------------

        # Print message residual conductance units
        {print("Residual conductance units: grams * s-1 * cm-2"); .} %>%

        # Residual conductance
        dplyr::mutate(residual_conductance = (transpiration_grams_per_sec_cm2 / median_vpd)*atmospheric_pressure_constant)

    return(base::data.frame(residual_conductance_df))

}
