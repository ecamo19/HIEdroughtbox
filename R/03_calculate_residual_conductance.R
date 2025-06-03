#' calculate_rate_of_change
#'
#' @description
#' This function calculates the rate of change (aka slope) of the weight of a
#' small branch measured in grams and time measured in seconds.
#'
#' @param droughtbox_data_reshaped Cleaned Dataframe loaded with the function
#' `reshape_droughtbox_data_reshaped()`.
#'
#' This dataframe must contain the following columns:
#'  strain_avg_1_microstrain_avg
#'  strain_avg_2_microstrain_avg
#'  strain_avg_3_microstrain_avg
#'  strain_avg_4_microstrain_avg
#'
#'  set_point_t_avg_avg
#'  vpd_avg_kpa_avg
#'  date_time
#'
#' @return A dataframe containing the strain_number, set_temperature and the
#' rate of change between weight and time.
#'
#' @examples
#' \dontrun{path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
#' calculate_rate_of_change(droughtbox_data = droughtbox_data)}
#'
#' @export
calculate_rate_of_change <- function(droughtbox_data_reshaped){

    rate_of_change <-

        # Data reshaped with the function reshape_droughtbox_data
        droughtbox_data_reshaped %>%

            # Print the units of the slope
            {print("Remember time units must be seconds and weights must be in grams"); .} %>%
            {print("Rate of change units: grams * s-1"); .} %>%

            # Get median climatic conditions ------------------------------------
            # Done at each temperature step
            dplyr::group_by(set_temperature) %>%

            dplyr::mutate(median_vdp  = stats::median(vpd_avg_kpa_avg),
                          median_rh   = stats::median(rh_avg_percent_avg),
                          median_temp = stats::median(tc_avg_deg_c_avg)) %>%

            dplyr::ungroup() %>%

            # Calculate the rate of change --------------------------------------

            # Create a nested dataframes excluding set_temperature,
            # strain_number.
            tidyr::nest(data = -c(median_vdp, median_rh, median_temp,
                                  string_number, set_temperature, vpd_control)) %>%

            # Create column with the slopes by strain_number, set_temperature
            dplyr::mutate(slope_grams_per_second = purrr::map(data,

                                                          # Calculate the slope
                                                          ~stats::coef(lm(string_weight_grams ~ time_seconds,
                                                                          data = .x))[["time_seconds"]])) %>%
            # Remove nested dataframes
            dplyr::select(-data) %>%

            # Unnest slope data
            tidyr::unnest(cols = slope_grams_per_second) %>%

            # Without this the code won't run
            dplyr::ungroup()

        # Print message if temperature measured and set temperature are different
        #base::ifelse(all(rate_of_change$set_temperature == rate_of_change$set_temperature),
        #             "all TRUE This is ok",
        #             print("set_temperature and set_temperature might be diffrent. Check data"))

    return(rate_of_change)
}

#' calculate_transpiration_rates
#'
#' @description
#' This function calculates the transpiration rate of a small branch following
#' equation 1 found the research paper titled 'The DroughtBox: A new tool for
#' phenotyping residual branch conductance and its temperature dependence during
#' drought' by Billon and colleagues.
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data()`.
#'
#' This dataframe must contain the following columns:
#'  strain_avg_1_microstrain_avg
#'  strain_avg_2_microstrain_avg
#'  strain_avg_3_microstrain_avg
#'  strain_avg_4_microstrain_avg
#'
#'  set_point_t_avg_avg
#'  vpd_avg_kpa_avg
#'  date_time
#'
#' @param leaf_and_branch_area_data Dataframe containing the leaf and/or branch
#' areas.
#'
#' This dataframe must contain the following columns:
#'  areas_m2
#'  strain_number
#'  set_temperature
#'  tree_id
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{path_droughtbox_leaf_branch_areas <- system.file("extdata",
#'                                                 "acacia_aneura_leaf_branch_areas.xlsx",
#'                                                 package = "HIEdroughtbox")
#'
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#' species_areas <- readxl::read_excel(path_droughtbox_leaf_branch_areas)
#'
#' calculate_residual_conductance(droughtbox_data = droughtbox_data,
#'                                leaf_and_branch_area_data = species_areas)}
#'
#' @export
calculate_transpiration_rates <- function(droughtbox_data,
                                          leaf_and_branch_area_data){

    # Validate input parameters -------------------------------------------------

    # Stop if droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Stop if droughtbox_data is not a data frame
    base::stopifnot("leaf_and_branch_area_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(leaf_and_branch_area_data))

    # Assert date column in droughtbox_data
    #checkmate::assert_date(droughtbox_data$date_time)

    # Assert time column in droughtbox_data
    #base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure the necessary data is in the dataframe
    # base::stopifnot("Missing weight columns in droughtbox_data. All weights should be included (4 in total)" = c("strain_avg_1_microstrain_avg",
    #                                                                                                              "strain_avg_2_microstrain_avg",
    #                                                                                                              "strain_avg_3_microstrain_avg",
    #                                                                                                              "strain_avg_4_microstrain_avg") %in% base::colnames(droughtbox_data))

    base::stopifnot("Missing set_point_t, vpd or/and, date_time colums" = c(#"set_point_t_avg_avg",
        "vpd_avg_kpa_avg",
        "date_time") %in% base::colnames(droughtbox_data))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in the leaf_and_branch_area_data" =  c("areas_m2",
                                                                            "string_number",
                                                                            #"set_temperature",
                                                                            "tree_id") %in% base::colnames(leaf_and_branch_area_data))

    # Calculate the rate of change between weight and time ----------------------
    slope_grams_per_second <- calculate_rate_of_change(droughtbox_data)

    # Estimate the transpiration rate -------------------------------------------
    transpiration_rate <-

        slope_grams_per_second %>%

        ## Merge leaf and branch areas data with slope data ---------------------
        dplyr::full_join(., leaf_and_branch_area_data,
                     by = c("string_number", "set_temperature", "vpd_control")) %>%

        # Calculate transpiration for single and double sided areas
        dplyr::mutate(transpiration_single_grams_per_sec_m2 =
                          -(.$slope_grams_per_second/(.$areas_m2)),

                      transpiration_double_grams_per_sec_m2 =
                          -(.$slope_grams_per_second/(.$double_sided_areas_m2))
                      ) %>%

        # Remove unused columns if present
        dplyr::select(-dplyr::any_of(c("set_vpd", "started_at",
                                       "number_of_leaves",
                                       "stem_dry_weight_mg",
                                       "leaf_dry_weight_mg"))) %>%

        # Arrange dataset
        dplyr::select(spcode, string_number, tree_id, vpd_control,
                      dplyr::everything()) %>%

        dplyr::group_by(set_temperature, tree_id, string_number, vpd_control) %>%
        dplyr::arrange(tree_id)

    return(transpiration_rate)
}

#' calculate_residual_conductance
#'
#' @description
#' This function calculates residual leaf conductance (gmin) or residual
#' branch conductance (gres) following equation 2 found the research paper
#' titled 'The DroughtBox: A new tool for phenotyping residual branch
#' conductance and its temperature dependence during drought' by Billon and
#' colleagues.
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data()`.
#'
#' This dataframe must contain the following columns:
#'  strain_avg_1_microstrain_avg
#'  strain_avg_2_microstrain_avg
#'  strain_avg_3_microstrain_avg
#'  strain_avg_4_microstrain_avg
#'
#'  set_point_t_avg_avg
#'  vpd_avg_kpa_avg
#'  date_time
#'
#' @param leaf_and_branch_area_data Dataframe containing the leaf and/or branch
#' areas.
#'
#' This dataframe must contain the following columns:
#'  areas_m2
#'  strain_number
#'  set_temperature
#'  tree_id
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe with the species_name, tree_id, strain_number,
#' set_temperature, transpiration_grams_per_sec_m2 and
#' median_vpd residual_conductance as columns.
#'
#' @examples
#' \dontrun{path_droughtbox_leaf_branch_areas <- system.file("extdata",
#'                                                 "acacia_aneura_leaf_branch_areas.xlsx",
#'                                                 package = "HIEdroughtbox")
#'
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#' species_areas <- readxl::read_excel(path_droughtbox_leaf_branch_areas)
#'
#' calculate_residual_conductance(droughtbox_data = droughtbox_data,
#'                                leaf_and_branch_area_data = species_areas)}
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
    #checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    #base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in droughtbox_data" =  c(#"strain_avg_1_microstrain_avg",
                                                              #"strain_avg_2_microstrain_avg",
                                                              #"strain_avg_3_microstrain_avg",
                                                              #"strain_avg_4_microstrain_avg",

                                                              #"set_point_t_avg_avg",
                                                              "vpd_avg_kpa_avg",
                                                              "date_time"
                                                              ) %in% base::colnames(droughtbox_data))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in the leaf_and_branch_area_data" =  c("areas_m2",
                                                                            "string_number",
                                                                            "tree_id"
                                                                            #"surface_branch_area_m2",
                                                                            #"set_temperature",
                                                                            ) %in% base::colnames(leaf_and_branch_area_data))

    # Get VPD parameter --------------------------------------------------------
    # This parameter will be later used in the estimation of gres.
    vpd_parameter <-

        droughtbox_data %>%

        # Select only the necessary variables
        dplyr::select(date_time,
                      vpd_avg_kpa_avg,
                      tc_avg_deg_c_avg) %>%

        # Change temperatures measured into discrete groups i.e if
        # tc_avg_deg_c_avg is between 53 and 56 code it as 55
        #dplyr::mutate(set_temperature = dplyr::case_when(
            #dplyr::between(tc_avg_deg_c_avg, 20, 26.5) ~ 25,
            #dplyr::between(tc_avg_deg_c_avg, 26.50001, 31.5) ~ 30,
            #dplyr::between(tc_avg_deg_c_avg, 31.50001, 36.5) ~ 35,
            #dplyr::between(tc_avg_deg_c_avg, 36.50001, 41.5) ~ 40,
            #dplyr::between(tc_avg_deg_c_avg, 41.50001, 46.5) ~ 45,
            #dplyr::between(tc_avg_deg_c_avg, 46.50001, 51.5) ~ 50,
            #dplyr::between(tc_avg_deg_c_avg, 51.50001, 60) ~ 55,
            #TRUE ~ tc_avg_deg_c_avg)) %>%

        # Rename variables
        #dplyr::mutate(
                      # Transform time to seconds
        #              time_seconds = (time - dplyr::first(time)),
        #              .keep = "unused") %>%

        # Group by temperature
        dplyr::group_by(set_temperature) %>%

        # Print message
        {print("Make sure VPD conditions were constant"); .} %>%

        # Get the median
        dplyr::summarise(median_vpd = stats::median(vpd_avg_kpa_avg))

    # Calculate transpiration rates --------------------------------------------
    transpiration <- calculate_transpiration_rates(droughtbox_data = droughtbox_data,
                                                  leaf_and_branch_area_data = leaf_and_branch_area_data)

    # Estimate residual conductance --------------------------------------------
    residual_conductance_df <-

        transpiration %>%

            # Add VPD parameter into the dataset
            dplyr::full_join(., vpd_parameter,
                             by = dplyr::join_by(set_temperature == set_temperature)) %>%

            # Print message residual conductance units
            {print("Residual conductance units: grams * s-1 * m-2 and mmols * s-1 * m-2"); .} %>%

            # Residual conductance in grams * s-1 * m-2 and
            dplyr::mutate(single_sided_residual_conductance_grams_s_m2 = (transpiration_single_grams_per_sec_m2 / median_vpd)*atmospheric_pressure_constant,
                          single_sided_residual_conductance_mmol_s_m2 = (single_sided_residual_conductance_grams_s_m2/18.02)*1000
                          ) %>%

            dplyr::mutate(double_sided_residual_conductance_grams_s_m2 = (transpiration_double_grams_per_sec_m2 / median_vpd)*atmospheric_pressure_constant,
                          double_sided_residual_conductance_mmol_s_m2 = (double_sided_residual_conductance_grams_s_m2 / 18.02)*1000
                      ) %>%


    return(base::data.frame(residual_conductance_df))

}
