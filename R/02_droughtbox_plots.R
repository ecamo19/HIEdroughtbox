#' plot_droughtbox_climatic_controls
#'
#' @description
#' This function plots the change across time of the climatic controls specified
#' by the user (shown in red) and the climatic controls measured inside of the
#' Droughtbox
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
#'
#' @export
#'
#' @examples
#' droughtbox_data <- read_hie_droughtbox_data("acacia_aneura_25c.dat")
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
                                                            "abs_h_avg_g_m3_avg",
                                                            "date_time"
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
        ggplot2::xlab("Time") +
        ggplot2::theme(legend.position = "bottom",
                       strip.text.x =  ggplot2::element_text(size = 22),
                       axis.text.y   = ggplot2::element_text(size = 22),
                       axis.text.x   = ggplot2::element_text(size = 22),
                       axis.title.y  = ggplot2::element_text(size = 22),
                       axis.title.x  = ggplot2::element_text(size = 22),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(size = 0.4,
                                                         colour = "black"),
                       panel.border = ggplot2::element_rect(colour = "black",
                                                            fill = NA,
                                                            size = 1))

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


#' plot_strains_weights
#' @description
#' This function displays the raw weights (grams) measured inside the Droughtbox
#'
#' @importFrom magrittr %>%
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#' @param show_strain String (i.e. "strain_1") or vector of strings
#' (c("strain_2", "strain_3")) indicating which strain to plot. Default is "all"
#' @return A ggplot2 object with the weight (grams) measured by each strain (4 in
#' total) inside the Droughtbox
#'
#' @export
#'
#' @examples
#' droughtbox_data <- read_hie_droughtbox_data("acacia_aneura_25c.dat")
#' plot_strains_weights(droughtbox_data)

plot_strains_weights <- function(droughtbox_data, show_strain = "all"){

    # Validate input dataset ---------------------------------------------------

    # Check that droughtbox_data is a dataframe
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("Missing columns in the dataframe" =  c("strain_avg_1_microstrain_avg",
                                                            "strain_avg_2_microstrain_avg",
                                                            "strain_avg_3_microstrain_avg",
                                                            "strain_avg_4_microstrain_avg",

                                                            "date_time"
                                                            ) %in% base::colnames(droughtbox_data))

    # Validate show_strain parameters
    options <- c("all", "strain_1", "strain_2","strain_3", "strain_4")

    # Stop if show_strain not in options
    if (!all(show_strain %in% options)) {
        stop("Invalid option for show_strain parameter. ",
             "Choose bewteen: all, strain_1, strain_2, strain_3 or strain_4. ",
             'For example c("strain_1","strain_2")')
    }

    # Dummy code for using all option
    if (show_strain == "all"){
        show_strain <- c("strain_1", "strain_2","strain_3", "strain_4")
    }

    # Create plot --------------------------------------------------------------
    droughtbox_data %>%

        # Select only the necessary variables for the plots
        dplyr::select(date_time,tare_count_smp,

                      # Variable 1
                      strain_avg_1_microstrain_avg,
                      strain_avg_2_microstrain_avg,
                      strain_avg_3_microstrain_avg,
                      strain_avg_4_microstrain_avg)  %>%

        # Reshape data into a long format
        tidyr::pivot_longer(!c(date_time, tare_count_smp),
                            names_to = "strains",
                            values_to = "strain_weight") %>%

        # Transform strain_number to factor type to keep consistent
        dplyr::mutate(strains = base::factor(strains)) %>%

        # Create new column with the new names for each strain
        dplyr::mutate(strain_number = dplyr::case_when(strains == "strain_avg_1_microstrain_avg"  ~ "strain_1",
                                                       strains == "strain_avg_2_microstrain_avg"  ~ "strain_2",
                                                       strains == "strain_avg_3_microstrain_avg"  ~ "strain_3",
                                                       strains == "strain_avg_4_microstrain_avg"  ~ "strain_4",
                                                       TRUE ~ strains)) %>%

        # Filter data based on the show_strain parameter
        dplyr::filter(strain_number %in% show_strain) %>%

        # Create plot
        ggplot2::ggplot(data = ., ggplot2::aes(x = date_time,
                                               y = strain_weight,
                                               colour = strain_number)) +


        ggplot2::geom_point() +

        # Add line with the mean value of the weights
        ggplot2::stat_smooth(ggplot2::aes(colour = strain_number),
                             se = FALSE) +

        # Try to implement the plot from ggplot2-book.org/annotations
        ggplot2::geom_text(ggplot2::aes(label = tare_count_smp)) +


        # Choose the theme
        ggplot2::theme_bw() +

        # Set y-scales as independent, I am leaving this line here for
        # remembering ggh4x library. It is useful!
        #ggh4x::facet_grid2(. ~variable, scales = "free_y", independent = "y") +

        # Edit x and y labs
        ggplot2::ylab("Strain weight (g)") +
        ggplot2::xlab("Time") +

        # Set the colors of each strain
        ggplot2::scale_color_manual(values = c("#eec000",
                                               "#cf544c",
                                               "#0175c3",
                                               "#878687")) +

        # Add legend at the bottom
        ggplot2::theme(legend.position = "bottom",
                       strip.text.x =  ggplot2::element_text(size = 25),
                       axis.text.y   = ggplot2::element_text(size = 25),
                       axis.text.x   = ggplot2::element_text(size = 25),
                       axis.title.y  = ggplot2::element_text(size = 25),
                       axis.title.x  = ggplot2::element_text(size = 25),
                       panel.grid.major.y = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(size = .4,
                                                         colour = "black"),
                       panel.border = ggplot2::element_rect(colour = "black",
                                                            fill = NA,
                                                            size = 1.3))
}
