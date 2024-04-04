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
#' @importFrom magrittr %>%
#' @return  Vector of strings with a length 30 elements
#' @export
#'
#' @examples
#' clean_droughtbox_colnames("acacia_aneura_25c.dat")

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

    return(.data)
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
#' @examples
#' read_hie_droughtbox_data("acacia_aneura_25c.dat")

read_hie_droughtbox_data <- function(path_droughtbox_data ){

    # Validate input dataset ---------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot(".dat file not found" = base::file.exists(path_droughtbox_data ) && !base::dir.exists(path_droughtbox_data))

    # Check is a .dat file
    base::stopifnot("Input must must be a .dat file" = tools::file_ext(path_droughtbox_data ) == 'dat' )

    # Check that first cell in .dat file is TOA5
    base::stopifnot("File not recognized. Check example files located in the data folder of the github package" =  "TOA5" %in% utils::read.table(path_droughtbox_data, nrows = 1, )[1,1])

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

        # Convert columns to the right format
        dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)),
                      time = hms::as_hms(time),
                      tare_count_smp = base::factor(tare_count_smp)) %>%

        # Convert character columns to numeric
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric)) %>%

        # Join date and time and create new column
        dplyr::mutate(date_time = lubridate::ymd_hms(paste(date, time))) %>%

        # Set date_column as the first column in the dataframe
        dplyr::select(tare_count_smp, date_time, dplyr::everything()) %>%

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

    return(tibble::as_data_frame(.data))
}

#' filter_droughtbox_data
#' @description
#' This function is meant to be used to removed chunks of data that is collected
#' when the droughtbox has not reach the climatic conditions desired.
#'
#' This functions does not remove individual observations
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#' @param from_start_date String indicating the initial Year, Month and Day to
#' filter in the dataset. It must have a YYYY-MM-DD format
#' @param to_end_date String indicating the final Year, Month and Day to filter
#' in the dataset. It must have a YYYY-MM-DD format
#' @param from_start_time String indicating the initial hour, minutes and seconds
#' to filter in the dataset. It must have a HH:MM:SS format
#' @param to_end_time String indicating the final hour, minutes and seconds
#' to filter in the dataset. It must have a HH:MM:SS format
#'
#' @return Dataframe with the selected dates and times
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' droughtbox_data <- read_hie_droughtbox_data("acacia_aneura_25c.dat")
#' filter_droughtbox_data(droughtbox_data,
#'                             from_start_date = "2024/03/04",
#'                             to_end_date = "2024/03/04",
#'                             from_start_time = NULL,
#'                             to_end_time = NULL)
#'
#' filter_droughtbox_data(droughtbox_data,
#'                             from_start_date = "2024/03/04",
#'                             to_end_date = "2024/03/04",
#'                             from_start_time = "12:51:00",
#'                             to_end_time = "12:52:00")
#'
#' filter_droughtbox_data(droughtbox_data,
#'                             from_start_date = NULL,
#'                             to_end_date = NULL,
#'                             from_start_time = "12:51:00",
#'                             to_end_time = "12:52:00")

filter_droughtbox_data <- function(droughtbox_data,
                                       from_start_date = NULL,
                                       to_end_date = NULL,
                                       from_start_time = NULL,
                                       to_end_time = NULL){

    print(crayon::cyan("Times must have a HH:MM:SS format i.e. 13:53:00"))
    print(crayon::cyan("Dates must have a YYYY-MM-DD format i.e. 1991-10-19"))

    # Validate input parameters ------------------------------------------------

    # Stop if all parameters are NULL
    if (all(is.null(from_start_date) & is.null(to_end_date) &
            is.null(from_start_time) & is.null(to_end_time))) {

        stop("from_start_date and to_end_date or from_start_time and to_end_time must be specified.")
    }

    # Stop if one of the times is not specified
    if (is.null(from_start_date) & is.null(to_end_date)) {

        if (any(is.null(from_start_time) | is.null(to_end_time))){
            stop("from_start_time and to_end_time must be both specified or set both to NULL. Time parameters must have a HH:MM:SS format i.e. 13:53:00")
        }
    }

    # Stop if one of the dates is not specified
    if (is.null(from_start_time) & is.null(to_end_time)) {

        if (any(is.null(from_start_date) | is.null(to_end_date))){
            stop("from_start_date and to_end_date must be both specified or set both to NULL. Date parameters must have a YYYY-MM-DD format i.e. 1991-10-19")
        }
    }

    # Stop if time parameters are NA
    if (!is.null(from_start_time) & !is.null(to_end_time)){

        if (is.na(hms::parse_hms(from_start_time)) | is.na(hms::parse_hms(to_end_time))) {
            stop("from_start_time or to_end_time are not in the correct format. Make sure the format is in 24h HH:MM:SS i.e. 13:53:00")
        }
    }

    # Stop if date parameters are NA
    if (all(!is.null(from_start_date) & !is.null(to_end_date))){

        if (is.na(lubridate::ymd(from_start_date)) | is.na(lubridate::ymd(to_end_date))) {
            stop("from_start_date or to_end_date are not in the correct format. Make sure the format is YYYY-MM-DD i.e. 1991-10-19")
        }
    }

    # Stop of droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Assert date column in droughtbox_data
    checkmate::assert_date(droughtbox_data$date)

    # Assert time column in droughtbox_data
    base::stopifnot("Time column should be of type hms/difftime" = "hms" %in% base::class(droughtbox_data$time))

    # Filter data --------------------------------------------------------------

    # Filter based on date parameters
    if (!is.null(c(from_start_date,to_end_date)) & is.null(c(from_start_time,to_end_time))){

        print(crayon::cyan(paste0("Filtering data by date from: ",
                                  from_start_date, " to: ", to_end_date)))

        # Convert parameters to the right format
        from_start_date <- lubridate::ymd(from_start_date )
        to_end_date <- lubridate::ymd(to_end_date)

        # Filter data
        droughtbox_data %>%
            dplyr::filter(date %in% (from_start_date:to_end_date)) %>%
            return(tibble::as_data_frame(.data))

    # Filter based on time parameters
    } else if(is.null(c(from_start_date,to_end_date)) & !is.null(c(from_start_time,to_end_time))){

        print(crayon::cyan(paste0("Filtering data by hour from: ",
                                  from_start_time, " to: ", to_end_time)))

        # Convert parameters to the right format
        from_start_time  <- hms::parse_hms(from_start_time)
        to_end_time <- hms::parse_hms(to_end_time)

        # Filter data
        droughtbox_data %>%
            dplyr::filter(time %in% (from_start_time:to_end_time)) %>%
            return(tibble::as_data_frame(.data))


    # Filter based on date and time parameters
    } else if(!is.null(c(from_start_date,to_end_date)) & !is.null(c(from_start_time,to_end_time))){

        print(crayon::cyan(paste0("Filtering data by hour and date from: ", from_start_date,
                                  " to: ", to_end_date)))

        # Join parameters
        from_start <- lubridate::ymd_hms(paste(from_start_date,from_start_time))

        # Create end values
        to_end <- lubridate::ymd_hms(paste(to_end_date,to_end_time))

        #
        droughtbox_data %>%
            dplyr::filter(date_time %in% (from_start:to_end)) %>%
            return(tibble::as_data_frame(.data))

    } else{

        # Break the code if some unknown condition is found
        stop('Filtering in filter_droughtbox_data function failed')
        }
}

#' clean_droughtbox_dataset
#' @description
#' This function removes wrong data points that are produced by the Droughtbox
#' after each taring process. First values lower than 0.1 grams are removed,
#' then the function reads the tare_count column in the dataset and selects the
#' values in the middle (we consider these as correct measurements) of each
#' group of tare_count
#'
#' Use the function `plot_raw_strains_weights` to visualize the wrong data points.
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#'
#' @importFrom magrittr %>%
#' @return A dataset
#' @export
#'
#' @examples
clean_droughtbox_dataset <- function(droughtbox_data){

    # Validate input parameters ------------------------------------------------
    # Stop of droughtbox_data is not a data frame
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("Missing columns in the dataframe" =  c("strain_avg_1_microstrain_avg",
                                                            "strain_avg_2_microstrain_avg",
                                                            "strain_avg_3_microstrain_avg",
                                                            "strain_avg_4_microstrain_avg",

                                                            "tare_count_smp"
                                                            ) %in% base::colnames(droughtbox_data))

    # Clean data ---------------------------------------------------------------

    droughtbox_data %>%

        # Remove values equal or lower than 0.3 grams across strain_avg vars
        dplyr::filter_at(dplyr::vars(starts_with('strain_avg')),
                         dplyr::any_vars(. >= 0.2)) %>%

        group_by()



    # Show how many points were removed

}
