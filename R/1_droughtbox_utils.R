#' Title
#'
#' @param path_data_droughtbox
#'
#' @return
#' @export
#'
#' @examples
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
#' @param path_data_droughtbox
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

