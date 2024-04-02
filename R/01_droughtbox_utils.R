#' clean_droughtbox_colnames
#' @description
#' This is an internal function meant to be used inside the
#' `clean_droughtbox_data` function.
#'
#' First it merges the the first two rows (which contain the units and the data
#' type of each column) of the .dat file. and then merges those merged rows with
#' each colname.
#'
#' The .dat file is downloaded from the the droughtbox
#'
#' The pattern of the new colname is varname_unit_data_type. For example
#' "air_tc_avg_deg_c_avg" the varname is air_tc_avg, the unit is deg_c and the
#' data_type is avg.
#'
#' Some colnames don`t have a units or data type. For example tare_count_sm,
#' where the varname is tare_count and the data_type is sm.
#'
#' @param path_droughtbox_data  String indicating the location of the .dat file in your computer
#'
#' @importFrom magrittr %>%
#' @return  Vector of strings with a length 30 elements
#' @export
#'
#' @examples clean_droughtbox_colnames("data/acacia_aneura_25c.dat")
#'
clean_droughtbox_colnames <- function(path_droughtbox_data){


    # Validate input dataset ---------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot(".dat file not found" = file.exists(path_droughtbox_data ) && !dir.exists(path_droughtbox_data ))

    # Check is a .dat file
    base::stopifnot("Input must must be a .dat file" = tools::file_ext(path_droughtbox_data ) == 'dat' )


    # Clean colnames -----------------------------------------------------------

    # Read data
    utils::read.table(path_droughtbox_data , header = TRUE, skip = 1,
                      sep = ",") %>%

    janitor::clean_names() %>%

    dplyr::filter(dplyr::row_number() %in% c(1, 2)) %>%

    # Merge all rows with units with data-type (i.e. Avg, min)
    dplyr::summarise(dplyr::across(tidyselect::where(is.character),
                                   stringr::str_c, collapse = "_")) %>%

    # Get the first row
    dplyr::filter(dplyr::row_number() == 1) %>%

    # Combine colnames with the first row
    stringr::str_c(base::colnames(.), ., sep = "-") %>%

    # Remove uppercase letters
    stringr::str_to_lower(.) %>%

    # Replace -_, spaces, - and / with a underscore
    mgsub::mgsub(., c("-_", " ", "-", "/"), c("_", "_", "_", "_")) %>%

    # Remove names ending with a underscore
    stringr::str_remove(., "\\_\\d?$") %>%

    return()
}

#'read_hie_droughtbox_data
#' @description
#' This function reads the raw .dat file downloaded from the droughtbox located
#' at the Hawkesbury Institute for the Environment
#'
#' @param path_droughtbox_data String indicating the location of the .dat file in your computer
#'
#' @return A dataframe with 25 columns
#' @importFrom magrittr %>%
#' @export
#'
#' @examples read_hie_droughtbox_data("data/acacia_aneura_25c.dat")
#'
#'
read_hie_droughtbox_data <- function(path_droughtbox_data ){

    # Validate input dataset ---------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot(".dat file not found" = base::file.exists(path_droughtbox_data ) && !base::dir.exists(path_droughtbox_data ))

    # Check is a .dat file
    base::stopifnot("Input must must be a .dat file" = tools::file_ext(path_droughtbox_data ) == 'dat' )


    # Read data ----------------------------------------------------------------
    utils::read.table(path_droughtbox_data , header = TRUE, skip = 1,
                          sep = ",") %>%

    # Substitute the old names with a clean ones
    magrittr::set_colnames(., clean_droughtbox_colnames(path_droughtbox_data )) %>%

    # Remove rows with units and comments
    dplyr::filter(!dplyr::row_number() %in% c(1, 2)) %>%

    # Change colnames to lowercase
    janitor::clean_names() %>%

    # separate timestamp column into date and time
    tidyr::separate(timestamp_ts, c("date", "time"), sep = " ") %>%

    # Convert date and time columns into a time format
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)), time = hms::as_hms(time)) %>%

    # Convert character columns to numeric
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric)) %>%

    # Join date and time and create new column
    dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%

    # Set date_column as the first column in the dataframe
    dplyr::select(date_time, dplyr::everything()) %>%

    # Remove not used variables
    dplyr::select(-c(record_rn, p_output_avg_avg, d_output_avg_avg,
                     i_avg_avg, batt_v_min_volts_min, i_output_avg_avg,
                     duty_cycle_avg_avg,

                     # Volt columns
                     vr1000_avg_1_mv_v_avg, vr1000_avg_2_mv_v_avg,
                     vr1000_avg_3_mv_v_avg,vr1000_avg_4_mv_v_avg,

                     # Hook temperature columns
                     t_sg_avg_1_avg, t_sg_avg_2_avg,
                     t_sg_avg_3_avg, t_sg_avg_4_avg)) %>%

    return(tibble::as_data_frame())
}

