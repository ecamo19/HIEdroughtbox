#' plot_droughtbox_climatic_controls
#'
#' @description
#' This function plots the change across time of the climatic controls specified
#' by the user (shown in red) and the climatic controls measured inside of the
#' Droughtbox
#'
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#'
#' @importFrom magrittr %>%
#'
#' @param cowplot Boolean indicating if plots should be arranged in one single
#' figure with 2 columns and 3 rows (TRUE) or if should be arranged individually
#'
#' @return A ggplot2 object with a total of 5 figures
#' @export
#'
#' @examples
#' droughtbox_data <- read_hie_droughtbox_data("data/acacia_aneura_25c.dat")
#' plot_droughtbox_climatic_controls(droughtbox_data, cowplot = TRUE)

plot_droughtbox_climatic_controls <- function(droughtbox_data, cowplot = TRUE){

    # Validate input dataset ---------------------------------------------------

    # Check that file exists and is not a folder
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Make sure data is in the dataframe
    base::stopifnot("Missing columns in the dataframe" =  c("set_point_vpd_avg_avg",
                                                            "set_point_t_avg_avg",
                                                            "set_point_rh_avg_avg",
                                                            "set_point_abs_h_avg_avg",

                                                            "air_tc_avg_deg_c_avg",
                                                            "rh_avg_percent_avg",
                                                            "tc_avg_deg_c_avg",
                                                            "vpd_avg_kpa_avg",
                                                            "abs_h_avg_g_m3_avg"

                                                            ) %in% base::colnames(droughtbox_data))

    # Create plots -------------------------------------------------------------

    # Function for positioning the label in each plot accordingly to each
    # y-axis variable
    ggplot_annotation_wrapper <- function(y){
        ggplot2::annotate(geom = 'text',

                          # Label position
                          x = base::min(droughtbox_data$date_time) + 2000,
                          y = y - 0.5,

                          # Text
                          label = "atop(set_conditions == red, measured_conditions == black)",
                          parse = TRUE,
                          col =  "blue",
                          size = 4.5)

    }

    # Create base plot
    base_plot <- ggplot2::ggplot(data = {{droughtbox_data}}) +
        ggplot2::theme_bw() +
        ggplot2::xlab("Time")

    # VPD
    vpd_plot <-
        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = vpd_avg_kpa_avg)) +

        # Set conditions for the box
        ggplot2::geom_point(ggplot2::aes(x = date_time,
                                          y = set_point_vpd_avg_avg),
                             color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$vpd_avg_kpa_avg)) +
        ggplot2::ylab("Vapour pressure deficit (kPa)")

    # Temperature
    temp_plot <-
        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = tc_avg_deg_c_avg)) +

        # Set conditions for the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_t_avg_avg),
                            color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$tc_avg_deg_c_avg)) +
        ggplot2::ylab("Temperature (degree Celsius)")

    # Air temperature
    air_temp_plot <-
        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = air_tc_avg_deg_c_avg)) +

        # Set conditions for the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_t_avg_avg),
                            color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$air_tc_avg_deg_c_avg)) +
        ggplot2::ylab("Air temperature (degree Celsius)")

    # Relative Humidity
    relative_humidity_plot <-
        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = rh_avg_percent_avg)) +

        # Set conditions for the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_rh_avg_avg),
                                color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$rh_avg_percent_avg)) +
        ggplot2::ylab("Relative humidity (%)")


    # Absolute humidity
    absolute_humidity_plot <-
        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = abs_h_avg_g_m3_avg)) +

        # Set conditions for the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_abs_h_avg_avg),
                            color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$abs_h_avg_g_m3_avg)) +
        ggplot2::ylab("Absolute humidity (g/m3)")

    # Return one figure with several plots
    if (cowplot == TRUE) {
        return(cowplot::plot_grid(ncol = 2,
                                  vpd_plot,
                                  temp_plot,
                                  absolute_humidity_plot,
                                  relative_humidity_plot,
                                  air_temp_plot))

    # Return each plot individually
    }else {
        print(vpd_plot)
        print(temp_plot)
        print(absolute_humidity_plot)
        print(relative_humidity_plot)
        print(air_temp_plot)
        }
}


#' plot_raw_strain_weight_data
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#' @return
#' @export
#'
#' @examples
plot_raw_strain_weight_data <- function(droughtbox_data){

}
