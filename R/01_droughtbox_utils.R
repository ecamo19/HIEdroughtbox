
#' clean_droughtbox_colnames
#'
#' @description
#' This is an internal function meant to be used inside the
#' `clean_droughtbox_data` function.
#'
#' First it merges the the first two rows (which contain the units and the data
#' type of each column) of the .dat file. and then merges those merged rows with
#' each colname.
#'
#' The .dat file is downloaded from the the droughtbox.
#'
#' The pattern of the new colname is varname_unit_data_type. For example
#' "air_tc_avg_deg_c_avg" the varname is air_tc_avg, the unit is deg_c and the
#' data_type is avg.
#'
#' Some colnames don`t have a units or data type.
#'
#' @param path_droughtbox_data_file String indicating the location of the .dat
#' file in your computer.
#'
#' @importFrom magrittr %>%
#'
#' @return Vector of strings with a length 30 elements.
#'
#' @examples
#' path_to_droughtbox_data_file <- system.file("extdata",
#'                                            "acacia_aneura_25c.dat",
#'                                             package = "HIEdroughtbox")
#'
#' clean_droughtbox_colnames(path_to_droughtbox_data_file)
#'
#' @noRd
#'
#' @keywords internal
clean_droughtbox_colnames <- function(path_droughtbox_data_file){

    # Validate input parameters ------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot(".dat file not found" = file.exists(path_droughtbox_data_file ) && !dir.exists(path_droughtbox_data_file ))

    # Check is a .dat file
    base::stopifnot("Input must must be a .dat file" = tools::file_ext(path_droughtbox_data_file ) == 'dat' )

    # Clean colnames -----------------------------------------------------------

    clean_colnames <-

        # Read data
        utils::read.table(path_droughtbox_data_file, header = TRUE, skip = 1,
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
        stringr::str_remove(., "\\_\\d?$")

    return(clean_colnames)
}

#' read_hie_droughtbox_data_file
#'
#' @description
#' This function reads the raw .dat file downloaded from the droughtbox located
#' at the Hawkesbury Institute for the Environment.
#'
#' @param path_droughtbox_data_file String indicating the location of the .dat
#' file in your computer.
#'
#' @return A dataframe with 25 columns.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' path_to_droughtbox_data_file <- system.file("extdata",
#'                                             "acacia_aneura_25c.dat",
#'                                             package = "HIEdroughtbox")
#'
#' read_hie_droughtbox_data_file(path_to_droughtbox_data_file)
#'
#' @export
read_hie_droughtbox_data_file <- function(path_droughtbox_data_file){

    # Validate input parameters ------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot(".dat file not found" = base::file.exists(path_droughtbox_data_file ) && !base::dir.exists(path_droughtbox_data_file))

    # Check is a .dat file
    base::stopifnot("Input must be a .dat file" = tools::file_ext(path_droughtbox_data_file ) == 'dat' )

    # Check that first cell in .dat file is TOA5
    base::stopifnot("File not recognized. Check example files located in the data folder of the github package" =  "TOA5" %in% utils::read.table(path_droughtbox_data_file, nrows = 1, )[1,1])

    # Read data ----------------------------------------------------------------

    dat_file <-

        utils::read.table(path_droughtbox_data_file, header = TRUE, skip = 1,
                      sep = ",") %>%

        # Substitute the old names with a clean ones
        magrittr::set_colnames(., clean_droughtbox_colnames(path_droughtbox_data_file )) %>%

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
                         t_sg_avg_3_avg, t_sg_avg_4_avg))

    return(base::data.frame(dat_file))
}

#' read_hie_droughtbox_data_folder
#'
#' @description
#' This function reads 2 or more raw .dat files downloaded from the
#' droughtbox located at the Hawkesbury Institute for the Environment.
#'
#' @param path_droughtbox_data_folder String indicating the location of a folder
#' containing 2 or more .dat files in your computer.
#'
#' @importFrom magrittr %>%
#'
#' @return A single list with 2 or more data frames.
#'
#' @examples
#' path_to_droughtbox_data_folder <- system.file("extdata",
#'                                                package = "HIEdroughtbox")
#'
#' read_hie_droughtbox_data_folder(path_to_droughtbox_data_folder)
#'
#' @export
read_hie_droughtbox_data_folder <- function(path_droughtbox_data_folder){

    # If path ends with /
    if (grepl("/$", path_droughtbox_data_folder)) {

        # Remove the / at the end
        path_droughtbox_data_folder <-  stringr::str_sub(path_droughtbox_data_folder,
                                                         end = -2)}
    else {
        # Leave it as it is
        path_droughtbox_data_folder <- path_droughtbox_data_folder
    }
    # Validate input parameters ------------------------------------------------

    # Check folder exits
    base::stopifnot("Folder not found" = base::dir.exists(path_droughtbox_data_folder))

    # Check there are several .dat files in the folder
    if (length(base::list.files(path = path_droughtbox_data_folder,
                                pattern = "\\.dat$")) <= 1) {

        stop("Folder MUST contain at least 2 or more .dat files")}

    else if (length(base::list.files(path = path_droughtbox_data_folder,
                                     pattern = "\\.dat$")) > 1) {

        # Get the names of each .dat file found in the folder
        file_names <- base::list.files(path = path_droughtbox_data_folder,
                                       pattern = "\\.dat$")}

    else {
        stop("Failed in read_all_hie_droughtbox_files function")}

    # Create list with all the dataframes --------------------------------------
    file_names <- file_names[!stringr::str_detect(file_names, "^dry_weight")]

    # Create list of dataframes
    list_with_data_frames <-

        file_names %>%

        # Merge path with each file name found in the folder
        base::paste0(path_droughtbox_data_folder, "/", .) %>%

        # Print message indicating the total number of files read
        {print(paste0("Reading: ", .)); .} %>%

        # Set the name for each dataframe in the list
        purrr::set_names(., file_names) %>%

        # Read each file
        purrr::map(read_hie_droughtbox_data_file) %>%

        # Print message indicating the total number of files read
        {print(paste0("Success! Total number of .dat files read: ", base::length(.))); .}

    return(list_with_data_frames)
}

#' create_empty_droughtbox_leaf_branch_areas_sheet
#'
#' @description
#' This function generates a CSV files filled with NA'S with the required
#' columns for recording the leaf and branch area data necessary to estimate
#' gmin or gres.
#'
#' @param save_empty_df_at String indicating the path where the empty datasheet
#' for recording the leaf and branch areas should be saved.
#'
#' @importFrom magrittr %>%
#'
#' @return Empty dataframe filled with NAs.
#'
#' @examples
#' \dontrun{create_empty_droughtbox_leaf_branch_areas_sheet("path/to/folder")}
#'
#' @export
create_empty_droughtbox_leaf_branch_areas_sheet <- function(save_empty_df_at = NULL){

    # Validate input parameters ------------------------------------------------

    # Create path to the current working directory in case not provided
    if (is.null(save_empty_df_at)) {
        path_output_file <- paste0(getwd(),
                                   '/empty_droughtbox_leaf_branch_areas_sheet.csv')
    }
    else {

        # Create path where the output file will be saved in case is provided
        path_output_file <- paste0(save_empty_df_at,
                                   '/empty_droughtbox_leaf_branch_areas_sheet.csv')
    }

    # Assert that path_output_file does NOT contain "//"
    if (stringr::str_detect(string = path_output_file,
                            pattern = "//")) {
        print(path_output_file)
        stop('// detetected in PATH. PATH should be "path/to" NOT "path/to/"')
    }

    # Check if a path is suited for creating an output file
    checkmate::assert_path_for_output(path_output_file)

    # Create empty dataframe ---------------------------------------------------

    empty_sheet <-
        tidyr::tibble(

            # Columns identifying each sample
            species_name = NA,
            sample_id = NA,
            strain_number = rep(seq(from = 1, to = 4), 7),
            set_temperature = as.integer(rep(seq(from = 25, to = 55, by = 5), 4)),

            # Columns to calculate the surface_branch_area_cm2
            branch_basal_diameter_mm = NA,
            branch_length_cm = NA,

            # Columns with information about the areas
            leaf_area_cm2 = NA,
            surface_branch_area_cm2 = NA,
            notes = NA) %>%

            # Arrange rows in dataframe
            dplyr::arrange(., set_temperature, strain_number) %>%

            # Print message indicating where the file will be saved
            {print(paste0("Empty CSV saved at: ",

                      # Print working directory where csv will be saved
                      path_output_file)); .}

    # Save Empty dataframe
    utils::write.csv(empty_sheet, file = path_output_file)
}

#' read_hie_droughtbox_leaf_branch_areas
#'
#' description
#' This function reads CSV files containing information about the leaf and/or
#' branch area of the samples measured in the droughtbox.
#'
#' To create a datasheet with the required information, run the
#' function `create_empty_droughtbox_leaf_branch_areas_sheet('path/to/folder')`.
#' with the path to the folder where the datasheet should be saved.
#'
#' The CSV file MUST contain the following columns:
#'
#' set_temperature: Integer indicating the temperature at which gmin/gres was
#' measured.
#'
#' strain_number: Integer indicating in which of the four hooks the the sample
#' was positioned.
#'
#' leaf_area: Float in cm2 with the total leaf area of the sample.
#'
#' The following columns are optional:
#'
#' tree_id: String with an unique code identifying each sample.
#'
#' surface_branch_area: Float in cm2 with the area of the branch without any
#' leaves attached. If not provided, the total surface_branch_area can be
#' approximated using the formula A = pi*radius*(length + radius), assuming that
#' the branch has a cone shape.
#'
#' branch_basal_diameter_mm: Float in millimeters indicating the basal diameter
#' of the sample.
#'
#' branch_length_cm: Float in centimeters indicating the total length of the
#' sample.
#'
#' param path_droughtbox_leaf_branch_areas String indicating the location of
#' the CSV file in your computer.
#'
#' return A dataframe.
#'
#' importFrom magrittr %>%
#'
#' examples
#' path_droughtbox_leaf_branch_areas <- system.file("extdata",
#'                                                 "acacia_aneura_leaf_branch_areas.xlsx",
#'                                                 package = "HIEdroughtbox")
#'
#' read_hie_droughtbox_leaf_branch_areas(path_droughtbox_leaf_branch_areas)
#'
#' export
# read_hie_droughtbox_leaf_branch_areas <- function(path_droughtbox_leaf_branch_areas){
#
#     # Validate input parameters ------------------------------------------------
#
#     # Check file is a csv
#     checkmate::assert_file_exists(path_droughtbox_leaf_branch_areas,
#                                   extension = "xlsx")
#
#     # Read data ----------------------------------------------------------------
#     data_leaf_branch_area <-
#
#         readxl::read_excel(path_droughtbox_leaf_branch_areas) %>%
#
#             # Remove notes column
#             dplyr::select(-notes)
#
#         # Remove rows that have NA's in the important variables?
#         # Implement if necessary
#
#     # Check that dataframe has the correct columns
#     base::stopifnot("Missing columns in the dataframe" =  c("sample_id",
#                                                             "set_temperature",
#                                                             "strain_number",
#                                                             "leaf_area_cm2"
#                                                             ) %in% base::colnames(data_leaf_branch_area))
#
#     # Stop if branch_basal_diameter_mm, branch_length_cm and are all provided
#     if (all(c("branch_basal_diameter_mm",
#               "branch_length_cm",
#               "surface_branch_area_cm2"
#               ) %in% colnames(data_leaf_branch_area)) == TRUE){
#
#         stop("Only provide branch_length_cm and branch_basal_diameter_mm columns OR just the surface_branch_area_cm2")
#     }
#
#     # Check if the columns have any NA's
#     variables_with_all_na <- sapply(data_leaf_branch_area, function(x) any(is.na(x)))
#
#     # Prepare data for calculating gmin/gres -----------------------------------
#
#     if ("branch_length_cm" %in% names(variables_with_all_na) &
#         "branch_basal_diameter_mm" %in% names(variables_with_all_na)) {
#
#         # Approximate surface_branch_area_cm2 using branch_basal_diameter_mm and
#         # branch_length_cm
#         if (variables_with_all_na["branch_basal_diameter_mm"] == FALSE &
#             variables_with_all_na["branch_length_cm"] == FALSE) {
#
#             leaf_branch_area_data <-
#                 data_leaf_branch_area %>%
#
#                     # Print message about branch_basal_diameter_mm being transformed and
#                     # then divided by two to get the radius in cm
#                     {print("branch_basal_diameter_mm converted to cm and divided by two to get the radius"); .} %>%
#                     dplyr::mutate(branch_basal_radius_cm = (branch_basal_diameter_mm*0.1)/2,
#                                 .keep = "unused")  %>%
#
#                     # Calculate surface_branch_area as the cone
#                     {print("surface_branch_area_cm2 aproximated using pi*radius*(length + radius) formula"); .} %>%
#                     dplyr::mutate(surface_branch_area_cm2 = pi*branch_basal_radius_cm*(branch_length_cm + branch_basal_radius_cm),
#                                 .keep = "unused")
#
#             return(base::data.frame(leaf_branch_area_data))}
#     }
#
#     # Select necessary columns if surface_branch_area_cm2 is provided
#     else if("surface_branch_area_cm2" %in% names(variables_with_all_na) &
#             variables_with_all_na["surface_branch_area_cm2"] == FALSE){
#
#         leaf_branch_area_data <-
#
#             data_leaf_branch_area %>%
#
#                 dplyr::select(species_name, sample_id, strain_number,
#                               set_temperature,
#
#                               # Areas
#                               leaf_area_cm2, surface_branch_area_cm2)
#
#         return(base::data.frame(leaf_branch_area_data))
#         }
#
#     # Stop if some unknown condition is met
#     else{
#         stop("Failed to read leaf and branch areas csv. Remember that branch_basal_diameter_mm, branch_length_cm or surface_branch_area_cm2 shouldn't conatin ANY NAs ")
#     }
# }

#' filter_droughtbox_data
#'
#' @description
#' This function is meant to be used to removed chunks of data that is collected
#' when the droughtbox has not reach the climatic conditions desired.
#'
#' This functions does not remove individual observations.
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`.
#'
#' @param from_start_date String indicating the initial Year, Month and Day to
#' filter in the dataset. It must have a YYYY-MM-DD format.
#'
#' @param to_end_date String indicating the final Year, Month and Day to filter
#' in the dataset. It must have a YYYY-MM-DD format.
#'
#' @param from_start_time String indicating the initial hour, minutes and
#' seconds to filter in the dataset. It must have a HH:MM:SS format.
#'
#' @param to_end_time String indicating the final hour, minutes and seconds
#' to filter in the dataset. It must have a HH:MM:SS format.
#'
#' @importFrom magrittr %>%
#'
#' @return Dataframe with the selected dates and times.
#'
#' @examples
#' \dontrun{create_empty_droughtbox_leaf_branch_areas_sheet("path/to/folder")}#' @importFrom magrittr %>%
#'
#' @examples
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
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
#'
#' @export
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
        from_start_date <- lubridate::ymd(from_start_date)
        to_end_date <- lubridate::ymd(to_end_date)

        # Filter data
        filtered_data <-

            droughtbox_data %>%
                dplyr::filter(date >= from_start_date & date <= to_end_date)

        return(base::data.frame(filtered_data))

    # Filter based on time parameters
    } else if(is.null(c(from_start_date,to_end_date)) & !is.null(c(from_start_time,to_end_time))){

        print(crayon::cyan(paste0("Filtering data by hour from: ",
                                  from_start_time, " to: ", to_end_time)))

        # Convert parameters to the right format
        from_start_time  <- hms::parse_hms(from_start_time)
        to_end_time <- hms::parse_hms(to_end_time)

        # Filter data
        filtered_data <-

            droughtbox_data %>%

                dplyr::filter(time >= from_start_time & time <= to_end_time)

        return(base::data.frame(filtered_data))

    # Filter based on date and time parameters
    } else if(!is.null(c(from_start_date,to_end_date)) & !is.null(c(from_start_time,to_end_time))){

        print(crayon::cyan(paste0("Filtering data by hour and date from: ", from_start_date,
                                  " to: ", to_end_date)))

        # Join parameters to create date_time
        from_start <- lubridate::ymd_hms(paste(from_start_date,from_start_time))

        to_end <- lubridate::ymd_hms(paste(to_end_date,to_end_time))

        # Filter data
        filtered_data <-

            droughtbox_data %>%

                dplyr::filter(date_time >= from_start & date_time <= to_end)

        return(base::data.frame(filtered_data))

    } else{

        # Break the code if some unknown condition is found
        stop('Filtering in filter_droughtbox_data function failed')}
}

#' clean_droughtbox_data
#'
#' @description
#' This function removes wrong data points that are produced by the Droughtbox
#' after each taring process. First it removes values lower than a `threshold`,
#' (which is in grams) and then it removes the first and last values of each
#' tare_count.
#'
#' The function `plot_raw_strains_weights` can be used to visualize the data
#' points might need to be removed.
#'
#' @param droughtbox_data Dataframe loaded with the function.
#' `read_hie_droughtbox_data_file`
#'
#' @param remove_n_observations Integer indicating the number of values that
#' need to be removed at the beginning each tare_count group.
#'
#' @param threshold Float indicating the threshold at which values should be
#' removed.
#'
#' @importFrom magrittr %>%
#'
#' @return A dataset.
#'
#' @examples
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
#' # Remove tare_counts without enough measurements
#' droughtbox_data <- droughtbox_data |> dplyr::filter(!tare_count_smp %in% c("13","14","28"))
#'
#' # Clean data
#' clean_droughtbox_data(droughtbox_data, remove_n_observations = 6)
#'
#' @export
clean_droughtbox_data <- function(droughtbox_data,
                                  remove_n_observations = 5,
                                  threshold = 0.2){

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

    # Make sure remove_n_observations is an integer
    checkmate::assert_int(remove_n_observations)

    # Check if each tare_group has enough measurements
    number_measurements_in_each_tare_group <-

        # Count the number of measurements in each tare
        droughtbox_data %>% dplyr::count(tare_count_smp) %>%

        # Label if tare count has enough data points to remove.
        # For example if the user wants to remove the first and last minute of
        # observations of each tare, the group should contain at least 13
        # measurements to return one value.
        dplyr::mutate(enough_observations =  dplyr::if_else(n > ((2*{{remove_n_observations}}) + 1),
                                                   TRUE, FALSE)) %>%

        # Get the tares that don't have enough measurements
        dplyr::filter(enough_observations == FALSE)

        # Stop if any FALSE is found
        if (nrow(number_measurements_in_each_tare_group) > 0) {

            print("tare_count group with not enough meaureaments found!")
            print(number_measurements_in_each_tare_group)
            print("Consider removing the tares or reduce the number of observations to be removed")

            # Remove object
            rm(number_measurements_in_each_tare_group)
            stop("tare_count group don't have enough meaureaments")
        }

    # Clean data ---------------------------------------------------------------
    cleaned_data <-

        droughtbox_data %>%

            # Remove values equal or lower than threshold across strain_avg vars
            dplyr::filter_at(dplyr::vars(dplyr::starts_with('strain_avg')),

                             # Remove values
                             dplyr::any_vars(. >= {{threshold}})) %>%

            # Identify the firsts and lasts values of each group (tare_count)
            dplyr::group_by(tare_count_smp) %>%

            # Remove the first 5 measurements at the beginning of each
            # tare_count and then return the next 5 observations.
            # For example, if a dataframe has 13 measurements and
            # remove_n_observations is equal to 5, then only the observations
            # 6,7,8,9 and 10 will be returned.
            dplyr::slice(({{remove_n_observations}} + 1):({{remove_n_observations}} + 5)) %>%

            # Print the total number of rows filtered
            {print(paste0("Total number of rows removed: ",

                      # Subtract the total minus the filtered
                      base::nrow(droughtbox_data) - base::nrow(.))); .}

    return(base::data.frame(cleaned_data))

}

#' merge_droughtbox_data
#'
#' @description
#' This function is meant to be used as the last step previously to calculate
#' the residual conductace. It takes any number of prevously cleaned .dat files
#' and merge them into a single data frame.
#'
#' @param ... n number of objects of class data.frame.
#'
#' @importFrom magrittr %>%
#'
#' @return A single data.frame object.
#'
#' @examples
#' \dontrun{merge_droughtbox_data(clean_data_1, clean_data_2, clean_data_n)}
#'
#' @export
merge_droughtbox_data <- function(...){

    # Store in a list all the objects passed in function parameters
    list_with_dataframes <- base::list(...)

    # Validate input parameters ------------------------------------------------

    # Stop if only one object is specified
    if (base::length(list_with_dataframes) <= 1 ) {
        base::stop("At least two dataframes should specified i.e. merge_droughtbox_data(clean_data_1, clean_data_2)")
    }

    # Stop if elements in the list are not dataframes

    # Get the class of each element in the list
    vector_with_classes <- c(purrr::map_chr(list_with_dataframes, class))

    # if not all are equal to data.frame then stop
    if (!all(vector_with_classes == "data.frame")) {

        # Print the classes found
        base::print(paste0('Object with class ', vector_with_classes, ' found'))

        # stop
        base::stop("All input dataframes should be of class data.frame")
    }

    # Stop if elements in the list have different number of columns

    # Get the number of columns of each element in the list
    vector_with_ncol <- purrr::map_dbl(list_with_dataframes, ncol)

    # if not all are equal to data.frame then stop
    if (length(unique(vector_with_ncol)) > 1) {

        # Print the number of columns of each data frame
        base::print(paste0('Dataframe with ', vector_with_ncol, ' columns found'))

        # stop
        base::stop("All input dataframes should have the same number of columns")
    }

    # Merge n number of objects given ------------------------------------------

    merged_droughtbox_data <-

        list_with_dataframes %>%

        # Merge data
        # reduce(left_join, by = "i")
        purrr::reduce(dplyr::full_join)

    return(merged_droughtbox_data)
}

#' split_data
#'
#'@description
#' Function for splitting a dataframe in two subgroups and running a linear
#' regression to each subgroup. This function is meant to be used
#' inside `calculate_residual_temperature_dependence_purrr`.
#'
#' @param data
#' Dataframe with two column temperature and residual conductance
#'
#' @param n
#' Number of rows for the firts regression. For example if the total number of
#' rows in data is 8 and n = 2 then the first dataframe will be of size 2 is and
#' the second dataframe for regression_2 will be of size 6.
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe with nested dataframes.
#'
#' @examples
#' \dontrun{split_data(data, n = 2)}
#'
#' @noRd
#'
#' @keywords internal
#'
#' @export
split_data <- function(data, n){

    # Create empty list
    subgroups  <- list()

    # Save split data into list
    subgroups[[1]] <- tidyr::nest(data[1:n,]) %>% dplyr::mutate(label = base::factor("regression_1"))
    subgroups[[2]] <- tidyr::nest(data[-(1:(n-1)),]) %>% dplyr::mutate(label = base::factor("regression_2"))

    # Return data
    return(dplyr::bind_rows(subgroups) %>%
               dplyr::select(label, data))

}

#' get_coefs
#'
#'@description
#' Function for  getting the slope and the intercept of a linear
#' regression to each subgroup. This function is meant to be used
#' inside `calculate_residual_temperature_dependence_purrr`.
#'
#' @param data
#' Dataframe with two columns, temperature and residual conductance
#'
#' @param transform_gmin_units Boolean indicating if residual conductance should
#' be converted (TRUE) from grams*cm-2*s-1 to mciro-moles*cm-2*s-1 or not *(FALSE)
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe of size 2x2
#'
#' @examples
#' \dontrun{get_coefs(data, transform_gmin_units = FALSE)}
#'
#' @noRd
#'
#' @keywords internal
#'
#' @export
get_coefs <- function(data, transform_gmin_units = FALSE){

    # Transform gmin units -----------------------------------------------------
    if(transform_gmin_units == TRUE){

        data$gmin_transformed <- (data$gmin / 18.02)*1000000
        print("gmin transformed from grams*cm-2*s-1 to mciro-moles*cm-2*s-1")

    } else{
        print("Make sure gmin units are micro-mol*cm-2*s-1")
        data$gmin_transformed <- data$gmin
    }

    # Validate input parameters ------------------------------------------------
    base::stopifnot("Missing gmin column. Check presence or spelling" = "gmin" %in% base::colnames(data))
    base::stopifnot("Missing temperature column. Check presence or spelling" = "temperature" %in% base::colnames(data))

    # Transform gmin to log10(gmin)*10 -----------------------------------------
    print("gmin transformed to log10(gmin)*10")
    data$gmin_transformed_log <- log10(data$gmin_transformed)*10

    # Main function ------------------------------------------------------------
    base::data.frame(value = stats::coef(stats::lm(gmin_transformed_log ~ temperature,
                                                   data = data))) %>%
        tibble::rownames_to_column("coef")  %>%
        dplyr::mutate(coef = case_when(

            coef == "(Intercept)" ~ "intercept",

            coef == "temperature" ~ "slope",

            TRUE ~ coef))
}

#' read_dry_weights_data_folder
#'
#' @description
#' This function reads the raw .dat file downloaded from the droughtbox located
#' at the Hawkesbury Institute for the Environment.
#'
#' @param path_droughtbox_data_folder containing
#'
#' @param paper_bag_group
#' Integer (either 1 or 2) indicating the paper bag group where samples were dried
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{read_dry_weights_data_folder()}
#'
#' @return A dataframe of with each sample dry
#' @export
read_dry_weights_data_folder <- function(path_droughtbox_data_folder){

    # Print
    base::cat("Make sure each filename start with dry_weight i.e. dry_weight_euc_saligna_dec_45c.dat")
    base::cat("\n")
    base::cat("\n")

    # Edit path to avoid errors -------------------------------------------------

    # If path ends with /
    if (grepl("/$", path_droughtbox_data_folder)) {

        # Remove the / at the end
        path_droughtbox_data_folder <-  stringr::str_sub(path_droughtbox_data_folder,
                                                         end = -2)}
    else {
        # Leave it as it is
        path_droughtbox_data_folder <- path_droughtbox_data_folder
    }

    # Validate input parameters ------------------------------------------------

    # Check folder exits
    base::stopifnot("Folder not found" = base::dir.exists(path_droughtbox_data_folder))

    # Check there are several .dat files in the folder
    if (length(base::list.files(path = path_droughtbox_data_folder,
                                pattern = "\\.dat$")) <= 1) {

        stop("Folder MUST contain at least 2 or more .dat files")}

    else if (length(base::list.files(path = path_droughtbox_data_folder,
                                     pattern = "\\.dat$")) > 1) {

        # Get the names of each .dat file found in the folder
        file_names <- base::list.files(path = path_droughtbox_data_folder,
                                       pattern = "\\.dat$")}

    else {
        stop("Failed in read_dry_weights_data")
    }

    # Create list with all the dataframes --------------------------------------

    # Filter the vector to get only the values that start with dry_weight
    dry_weight_filnames <- file_names[stringr::str_detect(file_names, "^dry_weight")]

    # Create list of dataframes
    list_with_data_frames <-

        dry_weight_filnames %>%

        # Merge path with each file name found in the folder
        base::paste0(path_droughtbox_data_folder, "/", .) %>%

        # Print message indicating the total number of files read
        {print(paste0("Reading: ", .)); .} %>%

        # Set the name for each dataframe in the list
        purrr::set_names(., dry_weight_filnames) %>%

        # Read each file
        purrr::map(read_hie_droughtbox_data_file) %>%

        # Print message indicating the total number of files read
        {print(paste0("Success! Total number of .dat files read: ", base::length(.))); .}

    return(list_with_data_frames)

}

#' reshape_droughtbox_data
#'
#' @description
#' This function transform the droughtbox data from wide to a long format
#'
#' @param droughtbox_data
#'
#' @return A dataframe
#'
#' @examples
#' \dontrun{reshape_droughtbox_data(euc_saligna_dec_merged_cleaned_data)}
#'
#' @export
reshape_droughtbox_data <- function(droughtbox_data){

    # Validate input parameters -------------------------------------------------

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

    # Reshape data  -------------------------------------------------------------
    droughtbox_data_reshaped <-

        # Transform the data into the right format
        droughtbox_data %>%

        # Select only the necessary variables calculating the rate of change
        dplyr::select(dplyr::any_of(c("time","tc_avg_deg_c_avg","date_time",
                                      "strain_avg_1_microstrain_avg",
                                      "strain_avg_2_microstrain_avg",
                                      "strain_avg_3_microstrain_avg",
                                      "strain_avg_4_microstrain_avg",
                                      "strain_avg_5_microstrain_avg",
                                      "strain_avg_6_microstrain_avg",
                                      "strain_avg_7_microstrain_avg",
                                      "strain_avg_8_microstrain_avg"))) %>%

        # Reshape data into a long format
        tidyr::pivot_longer(!c(time, tc_avg_deg_c_avg, date_time),

                            # Create new columns
                            names_to = "strings",
                            values_to = "string_weight_grams") %>%

        # Create new column with the new names for each strain
        dplyr::mutate(string_number = dplyr::case_when(strings == "strain_avg_1_microstrain_avg"  ~ "1",
                                                       strings == "strain_avg_2_microstrain_avg"  ~ "2",
                                                       strings == "strain_avg_3_microstrain_avg"  ~ "3",
                                                       strings == "strain_avg_4_microstrain_avg"  ~ "4",
                                                       strings == "strain_avg_5_microstrain_avg"  ~ "5",
                                                       strings == "strain_avg_6_microstrain_avg"  ~ "6",
                                                       strings == "strain_avg_7_microstrain_avg"  ~ "7",
                                                       strings == "strain_avg_8_microstrain_avg"  ~ "8",
                                                       TRUE ~ strings),
                      # Remove unused col
                      .keep = "unused") %>%

        # Change temperatures measured into discrete groups i.e if
        # tc_avg_deg_c_avg is between 53 and 56 code it as 55
        dplyr::mutate(temperature_measured = dplyr::case_when(
            dplyr::between(tc_avg_deg_c_avg, 20, 26.5) ~ 25,
            dplyr::between(tc_avg_deg_c_avg, 26.50001, 31.5) ~ 30,
            dplyr::between(tc_avg_deg_c_avg, 31.50001, 36.5) ~ 35,
            dplyr::between(tc_avg_deg_c_avg, 36.50001, 41.5) ~ 40,
            dplyr::between(tc_avg_deg_c_avg, 41.50001, 46.5) ~ 45,
            dplyr::between(tc_avg_deg_c_avg, 46.50001, 51.5) ~ 50,
            dplyr::between(tc_avg_deg_c_avg, 51.50001, 60) ~ 55,
            TRUE ~ tc_avg_deg_c_avg)) %>%

        # Step done for transforming time to seconds
        dplyr::group_by(string_number, temperature_measured) %>%

        # Transform columns
        dplyr::mutate(
            temperature_measured = as.integer(temperature_measured),
            string_number = as.integer(string_number),

            # Get time in seconds
            time_seconds = (time - dplyr::first(time)), .keep = "unused") %>%

        # Organize columns
        select(date_time, string_number, tc_avg_deg_c_avg, temperature_measured, everything())

    return(droughtbox_data_reshaped)

}


