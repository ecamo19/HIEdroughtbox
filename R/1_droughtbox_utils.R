#' clean_droughtbox_colnames
#' @description
#' This is an internal function meant to be used inside the `clean_droughtbox_data`
#' function.
#'
#' First it merges the the first two rows (which contain the units and the data
#' type of each column) of the .dat file downloaded from the the droughtbox and
#' then merges those merged rows with s each colname.
#'
#' The pattern of the new colname is varname_unit_data_type. For example
#' "air_tc_avg_deg_c_avg" the varname is air_tc_avg, the unit is deg_c and the
#' data_type is avg.
#'
#' Some colnames don`t have a units or data type. For example tare_count_sm,
#' where the varname is tare_count and the data_type is sm.
#'
#'
#'
#' @param path_data_droughtbox String indicating the location of the .dat file in your computer
#'
#' @return  Vector of strings of length 30
#' @export
#'
#' @examples clean_droughtbox_colnames(path/to/file.dat)
#'
clean_droughtbox_colnames <- function(path_data_droughtbox){


    # Validate input dataset ---------------------------------------------------

    # Check is a .dat file
    stopifnot("Input must must be a .dat file" = tools::file_ext(path_data_droughtbox) == 'dat' )

    # Clean colnames -----------------------------------------------------------

    # Read data
    utils::read.table(path_data_droughtbox, header = TRUE, skip = 1,
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

    return(.)
}



#' Title
#'
#' @param path_data_droughtbox String indicating the location of the .dat file in your computer
#'
#' @return
#' @export
#'
#' @examples
clean_droughtbox_data <- function(path_data_droughtbox){


    # Read data
    utils::read.table(path_data_droughtbox, header = TRUE, skip = 1,
                          sep = ",") %>%

    janitor::clean_names() %>%
    magrittr::set_colnames(., clean_droughtbox_colnames(path_data_droughtbox)) %>%
    return()
}

