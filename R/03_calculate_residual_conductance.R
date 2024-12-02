#' calculate_rate_of_change
#'
#' @description
#' This function calculates the rate of change (aka slope) of the weight of a
#' small branch measured in grams and time measured in seconds.
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
#' @return A dataframe containing the strain_number, set temperature and the
#' rate of change between weight and time.
#'
#' @examples
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
#' calculate_rate_of_change(droughtbox_data = droughtbox_data)
#'
#' @export
calculate_rate_of_change <- function(droughtbox_data){

    # Validate input parameters ------------------------------------------------

    # Stop if droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Assert date column in droughtbox_data
    checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing date_time or tare_count_smp column" = c("date_time",
                                                                     "tare_count_smp"
                                                                     ) %in% base::colnames(droughtbox_data))

    base::stopifnot("Missing tc_avg_deg_c_avg, vpd or/and, date_time colums" = c("tc_avg_deg_c_avg",
                                                                            "vpd_avg_kpa_avg",
                                                                            "date_time") %in% base::colnames(droughtbox_data))

    # Calculate the rate of change ---------------------------------------------
    rate_of_change <-

        # Transform the data into the right format
        droughtbox_data %>%

        # Select only the necessary variables calculating the rate of change
        dplyr::select(dplyr::any_of(c("time","tc_avg_deg_c_avg","set_point_t_avg_avg",
                                      "strain_avg_1_microstrain_avg",
                                      "strain_avg_2_microstrain_avg",
                                      "strain_avg_3_microstrain_avg",
                                      "strain_avg_4_microstrain_avg",
                                      "strain_avg_5_microstrain_avg",
                                      "strain_avg_6_microstrain_avg",
                                      "strain_avg_7_microstrain_avg",
                                      "strain_avg_8_microstrain_avg"))) %>%
        # Reshape data into a long format
        tidyr::pivot_longer(!c(time, tc_avg_deg_c_avg, set_point_t_avg_avg),

                            # Create new columns
                            names_to = "strains",
                            values_to = "strain_weight") %>%

        # Create new column with the new names for each strain
        dplyr::mutate(strain_number = dplyr::case_when(strains == "strain_avg_1_microstrain_avg"  ~ "1",
                                                       strains == "strain_avg_2_microstrain_avg"  ~ "2",
                                                       strains == "strain_avg_3_microstrain_avg"  ~ "3",
                                                       strains == "strain_avg_4_microstrain_avg"  ~ "4",
                                                       strains == "strain_avg_5_microstrain_avg"  ~ "5",
                                                       strains == "strain_avg_6_microstrain_avg"  ~ "6",
                                                       strains == "strain_avg_7_microstrain_avg"  ~ "7",
                                                       strains == "strain_avg_8_microstrain_avg"  ~ "8",
                                                       TRUE ~ strains),
                      # Remove unused col
                      .keep = "unused") %>%

        # Change temperatures measured into discrete groups i.e if
        # tc_avg_deg_c_avg is between 53 and 56 code it as 55
        dplyr::mutate(temperature_measured = dplyr::case_when(
            dplyr::between(tc_avg_deg_c_avg, 20, 26) ~ 25,
            dplyr::between(tc_avg_deg_c_avg, 26.00001, 31) ~ 30,
            dplyr::between(tc_avg_deg_c_avg, 31.00001, 36) ~ 35,
            dplyr::between(tc_avg_deg_c_avg, 36.00001, 41) ~ 40,
            dplyr::between(tc_avg_deg_c_avg, 41.00001, 46) ~ 45,
            dplyr::between(tc_avg_deg_c_avg, 46.00001, 51) ~ 50,
            dplyr::between(tc_avg_deg_c_avg, 51.00001, 60) ~ 55,
            TRUE ~ tc_avg_deg_c_avg)) %>%

        # Step done for transforming time to seconds
        dplyr::group_by(strain_number, set_point_t_avg_avg,temperature_measured) %>%

        # Transform columns
        dplyr::mutate(set_temperature = as.integer(set_point_t_avg_avg),
                      temperature_measured = as.integer(temperature_measured),
                      strain_number = as.integer(strain_number),

                      # Get time in seconds
                      time_seconds = (time - dplyr::first(time)),

                      .keep = "unused") %>%

        # Calculate the rate of change -----------------------------------------

        # Create a nested dataframes by strain_number, set_temperature
        tidyr::nest(data = -c(strain_number, set_temperature,temperature_measured)) %>%

        # Print the units of the slope
        {print("Make sure the time units are in seconds and the weights are in grams"); .} %>%
        {print("Rate of change units: grams * s-1"); .} %>%

        # Create column with the slopes by strain_number, set_temperature
        dplyr::mutate(slope_grams_per_second = purrr::map(data,

                                                          # Calculate the slope
                                                          ~stats::coef(lm(strain_weight ~ time_seconds,
                                                                   data = .x))[["time_seconds"]])) %>%
        # Remove nested dataframes
        dplyr::select(-data) %>%

        # Unnest slope data
        tidyr::unnest(cols = slope_grams_per_second) %>%

        # Print message if positive slope found
         {dplyr::if_else(.$set_temperature == .$temperature_measured, "This is OK",
                         print("set_temperature and temperature_measured are different. Check the data"),
                         ); .} %>%

        # Without this the code won't run
        dplyr::ungroup()

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
#'  areas_cm2
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
    # base::stopifnot("Missing weight columns in droughtbox_data. All weights should be included (4 in total)" = c("strain_avg_1_microstrain_avg",
    #                                                                                                              "strain_avg_2_microstrain_avg",
    #                                                                                                              "strain_avg_3_microstrain_avg",
    #                                                                                                              "strain_avg_4_microstrain_avg") %in% base::colnames(droughtbox_data))

    base::stopifnot("Missing set_point_t, vpd or/and, date_time colums" = c("set_point_t_avg_avg",
                                                                            "vpd_avg_kpa_avg",
                                                                            "date_time") %in% base::colnames(droughtbox_data))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in the leaf_and_branch_area_data" =  c("areas_cm2",
                                                                            "strain_number",
                                                                            "set_temperature",
                                                                            "tree_id") %in% base::colnames(leaf_and_branch_area_data))

    # Calculate the rate of change between weight and time ---------------------
    slope_grams_per_second <- calculate_rate_of_change(droughtbox_data)

    # Estimate the transpiration rate ------------------------------------------
    transpiration_rate <-

        slope_grams_per_second %>%

            ## Merge leaf and branch areas data with slope data ----------------
            dplyr::full_join(., leaf_and_branch_area_data,
                             by = c("strain_number", "set_temperature")) %>%

            # Calculate transpiration
            dplyr::mutate(transpiration_grams_per_sec_cm2 =
                             -(.$slope_grams_per_second/(.$areas_cm2))) %>%

            # Remove unused columns if present
            dplyr::select(-dplyr::any_of(c("set_vpd", "started_at",
                                           "number_of_leaves",
                                           "stem_dry_weight_mg",
                                           "leaf_dry_weight_mg"))) %>%

            # Arrange dataset
            dplyr::select(spcode, tree_id, dplyr::everything())

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
#'  areas_cm2
#'  strain_number
#'  set_temperature
#'  tree_id
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe with the species_name, tree_id, strain_number,
#' set_temperature, transpiration_grams_per_sec_cm2 and
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
    checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in droughtbox_data" =  c(#"strain_avg_1_microstrain_avg",
                                                              #"strain_avg_2_microstrain_avg",
                                                              #"strain_avg_3_microstrain_avg",
                                                              #"strain_avg_4_microstrain_avg",

                                                              "set_point_t_avg_avg",
                                                              "vpd_avg_kpa_avg",
                                                              "date_time"
                                                              ) %in% base::colnames(droughtbox_data))

    # Make sure the necessary data is in the dataframe
    base::stopifnot("Missing columns in the leaf_and_branch_area_data" =  c("areas_cm2",
                                                                            #"surface_branch_area_cm2",
                                                                            "strain_number",
                                                                           "set_temperature",
                                                                            "tree_id"
                                                                            ) %in% base::colnames(leaf_and_branch_area_data))

    # Get VPD parameter --------------------------------------------------------
    # This parameter will be later used in the estimation of gres.
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

    # Calculate transpiration rates --------------------------------------------
    transpiration <- calculate_transpiration_rates(droughtbox_data = droughtbox_data,
                                                                     leaf_and_branch_area_data = leaf_and_branch_area_data)

    # Estimate residual conductance --------------------------------------------
    residual_conductance_df <-

        transpiration %>%

            # Add VPD parameter into the dataset
            dplyr::full_join(., vpd_parameter, by = c("set_temperature")) %>%

            # Print message residual conductance units
            {print("Residual conductance units: grams * s-1 * cm-2"); .} %>%

            # Residual conductance in grams * s-1 * cm-2 and
            dplyr::mutate(residual_conductance_grams_s_cm = (transpiration_grams_per_sec_cm2 / median_vpd)*atmospheric_pressure_constant,
                          residual_conductance_micro_mol_s_cm = (residual_conductance_grams_s_cm/18.02)*1000000
                          )

    return(base::data.frame(residual_conductance_df))

}
