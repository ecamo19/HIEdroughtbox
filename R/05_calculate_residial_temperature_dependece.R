#' calculate_residual_temperature_dependece
#'
#' @description
#' This function calculates a slope and a intercept with a varying number of
#' data points in a range from two to eight. For example it calculates a slope
#' using the first two data points (slope_1, intercept_1) and then uses the rest
#' for calculating a second slope and intercept (slope_1, intercept_1).
#'
#' @param gmin Column containing the residual conductance values. The units
#' should be micro-mol*cm-2*s-1. The necessary data can be obtained with the
#' `calculate_residual_conductance` function.
#'
#' @param temperature Column containing the temperature at which gmin was
#' measured values. The units should be degrees Celsius. The necessary data can
#' be obtained with the `calculate_residual_conductance` function.
#'
#' @return A dataframe with different parameters. Use the function
#' `plot_arrhenius` to select the best set of parameters according to Billon
#' et al 2020.
#'
#' @examples
#' path_to_tp_data <- system.file("extdata",
#'                             "tp_data.csv",
#'                              package = "HIEdroughtbox")
#'
#'tp_data <- read.csv(path_to_tp_data, header = TRUE)
#'
#'calculate_residual_temperature_dependece(gmin = tp_data$gmin,
#'                                         temperature = tp_data$temperature)
#'
#' @export
calculate_residual_temperature_dependece <- function(gmin, temperature){
    print("Make sure gmin units are micro-mol*cm-2*s-1")

    # Validate input parameters ------------------------------------------------

    # Validate input types
    checkmate::assert(checkmate::checkClass(temperature, "integer"))
    checkmate::assert(checkmate::checkClass(gmin, "numeric"))

    # Validate Temperature values are within range
    checkmate::assert_numeric(x = temperature, any.missing = T, lower = 20,
                              upper = 60)

    # Range defined using fig.2 from the paper: On the minimum leaf
    # conductance: its role in models of plant water use, and ecological and
    # environmental controls
    checkmate::assert_numeric(x = gmin, any.missing = T, lower = 0.1,
                              upper = 100)

    # Main function ------------------------------------------------------------

    # Create empty list for storing parameters
    first_slope_list  <- list()
    second_slope_list  <- list()

    ## Join input parameters ---------------------------------------------------
    # Done in this way for controlling the colnames
    print("Transforming gmin to log10(gmin)*10")

    tp_data <- data.frame(base::cbind(gmin_transformed = log10(gmin)*10,
                    temperature = temperature))

    ## Calculate the slope at different points ---------------------------------
    for(each_row in 1:nrow(tp_data)){

        if (each_row > 1 && each_row < 8) {

            ## First slope -----------------------------------------------------
            first_slope <-

                # Get the slopes and intercepts
                base::as.data.frame(base::summary(stats::lm(gmin_transformed ~ temperature,
                                         data = tp_data[1:each_row,]))$coefficients) %>%

                janitor::clean_names() %>%
                dplyr::select(estimate) %>%
                tibble::rownames_to_column("coef") %>%

                # Rename coefficients
                dplyr::mutate(coef = dplyr::case_when(
                                        coef == "(Intercept)" ~ "intercept_1",
                                        coef == "temperature" ~ "slope_1",
                    TRUE ~ coef)) %>%

                tibble::add_column(number_of_values = each_row)

            # Append coefficients to list
            first_slope_list[[each_row]] <- first_slope

            ## Second slope ----------------------------------------------------
            second_slope  <-

                # Get the slopes and intercepts
                base::as.data.frame(base::summary(stats::lm(gmin_transformed ~ temperature,

                                         # Done in this way for including the
                                         # last data point
                                         data = tp_data[-(1:(each_row-1)),]))$coefficients) %>%

                janitor::clean_names() %>%
                dplyr::select(estimate) %>%
                tibble::rownames_to_column("coef") %>%

                dplyr::mutate(coef = dplyr::case_when(
                                        coef == "(Intercept)" ~ "intercept_2",
                                        coef == "temperature" ~ "slope_2",
                    TRUE ~ coef)) %>%

                tibble::add_column(number_of_values = each_row)

            # Append coefficients to list
            second_slope_list[[each_row]] <- second_slope
        }
    }

    # Flat lists
    clean_data <- base::rbind(dplyr::bind_rows(first_slope_list),
                             dplyr::bind_rows(second_slope_list)) %>%

                  dplyr::arrange(number_of_values) %>%
                  dplyr::select(number_of_values, coef, estimate) %>%
                  tidyr::pivot_wider(names_from = coef, values_from = estimate) %>%

                  # Calculate parameters
                  dplyr::mutate(q10_a = 10^slope_1,
                              q10_b = 10^slope_2,
                              gmin_20 = 10^((slope_1*20 + intercept_1)/10),
                              tp_celsius = (intercept_1 - intercept_2 )/(slope_2- slope_1))

    return(clean_data)
}
