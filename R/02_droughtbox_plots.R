#' plot_droughtbox_climatic_controls
#'
#' @description
#' This function plots the change across time of the climatic controls specified
#' by the user (shown in red) and the climatic controls measured inside of the
#' Droughtbox.
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data`
#'
#' @importFrom magrittr %>%
#'
#' @param cowplot Boolean indicating if plots should be arranged in one single
#' figure with 2 columns and 3 rows (TRUE) or if should be arranged
#' individually.
#'
#' @return A ggplot2 object with a total of 5 figures.
#'
#'
#' @examples
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
#' plot_droughtbox_climatic_controls(droughtbox_data, cowplot = TRUE)
#'
#' @export
plot_droughtbox_climatic_controls <- function(droughtbox_data, cowplot = TRUE){

    # Validate input parameters ------------------------------------------------

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
    base_plot <-

        ggplot2::ggplot(data = {{droughtbox_data}}) +
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
                                                            size = 1)) +
        # Add median and set temp to the title
        ggplot2::ggtitle(stringr::str_c("Set temperature: ", {{droughtbox_data}}$set_point_t_avg_avg,
                                        " Median temperature: ", stats::median({{droughtbox_data}}$tc_avg_deg_c_avg)))

    ## VPD plot ----------------------------------------------------------------
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

    ## Temperature plot --------------------------------------------------------
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

    ## Air temperature plot ----------------------------------------------------
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

    ## Relative Humidity plot --------------------------------------------------
    relative_humidity_plot <-

        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = rh_avg_percent_avg)) +

        # Set conditions for the box
        #ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_rh_avg_avg),
        #                        color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$rh_avg_percent_avg)) +
        ggplot2::ylab("Relative humidity (%)")


    ## Absolute humidity plot --------------------------------------------------
    absolute_humidity_plot <-

        base_plot +

        # Measured conditions inside the box
        ggplot2::geom_point(ggplot2::aes(x = date_time, y = abs_h_avg_g_m3_avg)) +

        # Set conditions for the box
        #ggplot2::geom_point(ggplot2::aes(x = date_time, y = set_point_abs_h_avg_avg),
        #                    color = "red") +

        # Add annotation
        ggplot_annotation_wrapper(y = base::max(droughtbox_data$abs_h_avg_g_m3_avg)) +
        ggplot2::ylab("Absolute humidity (g/m3)")

    # Return one figure with several plots
    if (cowplot == TRUE) {
        return(cowplot::plot_grid(ncol = 2, vpd_plot, temp_plot,
                                  absolute_humidity_plot, relative_humidity_plot,
                                  air_temp_plot))}

    # Return each plot individually
    else {
        print(vpd_plot)
        print(temp_plot)
        print(absolute_humidity_plot)
        print(relative_humidity_plot)
        print(air_temp_plot)}
}

#' plot_strains_weights
#'
#' @description
#' This function displays the raw weights (grams) measured inside the
#' Droughtbox.
#'
#' @importFrom magrittr %>%
#'
#' @param droughtbox_data Dataframe loaded with the function.
#' `read_hie_droughtbox_data`
#'
#' @param show_strain String (i.e. "strain_1") or vector of strings
#' (c("strain_2", "strain_3")) indicating which strain to plot.
#' Default is "all".
#'
#' @param time_breaks String indicating the resolution at which the x-axis
#' should show the tick marks. Choose one of "sec", "min", "hour", "day",
#' "week", "month", "year".
#'
#' @param show_tare_group Boolean (TRUE/FALSE) indicating if data should
#' be shown as points or as labels with the tare_count to which they
#' belong to.
#'
#' @return A ggplot2 object with the weight (grams) measured by each strain (4 in
#' total) inside the Droughtbox.
#'
#' @examples
#' \dontrun{
#' path_to_droughtbox_data <- system.file("extdata",
#'                             "acacia_aneura_25c.dat",
#'                             package = "HIEdroughtbox")
#'
#' droughtbox_data <- read_hie_droughtbox_data_file(path_to_droughtbox_data)
#'
#' plot_strains_weights(droughtbox_data)
#'}
#' @export
plot_strains_weights <- function(droughtbox_data, show_strain = "all",
                                 show_tare_group = TRUE,
                                 time_breaks = "10 min"){

    # Validate input parameters ------------------------------------------------

    # Check that droughtbox_data is a dataframe
    base::stopifnot("droughtbox_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(droughtbox_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("Missing columns in the dataframe" =  c(
                                                            # "strain_avg_1_microstrain_avg",
                                                            # "strain_avg_2_microstrain_avg",
                                                            # "strain_avg_3_microstrain_avg",
                                                            # "strain_avg_4_microstrain_avg",
                                                            #
                                                            # "strain_avg_5_microstrain_avg",
                                                            # "strain_avg_6_microstrain_avg",
                                                            # "strain_avg_7_microstrain_avg",
                                                            # "strain_avg_8_microstrain_avg",

                                                            "date_time"
    ) %in% base::colnames(droughtbox_data))

    # Validate show_strain parameters
    checkmate::assert_character(time_breaks)

    # Validate show_strain parameters
    checkmate::assert_character(show_strain)

    options <- c("all", "strain_1", "strain_2","strain_3", "strain_4",
                 "strain_5", "strain_6", "strain_7", "strain_8")

    # Stop if show_strain not in options
    if (!all(show_strain %in% options)) {
        stop("Invalid option for show_strain parameter. ",
             "Choose bewteen: all, strain_1, strain_2, strain_3,strain_4,
             strain_5, strain_6, strain_7 or strain_8, ",
             'For example c("strain_1","strain_8")')
    }

    # Dummy code for using all option
    if (show_strain == "all"){
        show_strain <- c("strain_1", "strain_2","strain_3", "strain_4",
                         "strain_5", "strain_6", "strain_7", "strain_8")
    }

    # Create plot --------------------------------------------------------------

    # Set specific colors to each strain so these won't change.
    strain_colors <- c(strain_1 = "#eec000",
                       strain_2 = "#cf544c",
                       strain_3 = "#0175c3",
                       strain_4 = "#878687",

                       strain_5 =  "#000000",
                       strain_6 =  "#008b28",
                       strain_7 =  "#8600b6",
                       strain_8 =  "#00a0ab"
                       )

    # Transform the data into the right format for the ggplot
    droughtbox_data %>%

        # Select only the necessary variables for the plots if these are present
        dplyr::select(dplyr::any_of(c("date_time", "tare_count_smp",
                                      "set_point_t_avg_avg", "tc_avg_deg_c_avg",
                                      "strain_avg_1_microstrain_avg",
                                      "strain_avg_2_microstrain_avg",
                                      "strain_avg_3_microstrain_avg",
                                      "strain_avg_4_microstrain_avg",
                                      "strain_avg_5_microstrain_avg",
                                      "strain_avg_6_microstrain_avg",
                                      "strain_avg_7_microstrain_avg",
                                      "strain_avg_8_microstrain_avg"))) %>%

        # Reshape data into a long format
        tidyr::pivot_longer(!c(date_time, tare_count_smp, set_point_t_avg_avg,
                               tc_avg_deg_c_avg),

                            # Create new columns
                            names_to = "strains",
                            values_to = "strain_weight") %>%

        # Transform strain_number to factor type to keep consistent
        dplyr::mutate(strains = base::factor(strains)) %>%

        # Create new column with the new names for each strain
        dplyr::mutate(strain_number = dplyr::case_when(strains == "strain_avg_1_microstrain_avg"  ~ "strain_1",
                                                       strains == "strain_avg_2_microstrain_avg"  ~ "strain_2",
                                                       strains == "strain_avg_3_microstrain_avg"  ~ "strain_3",
                                                       strains == "strain_avg_4_microstrain_avg"  ~ "strain_4",

                                                       strains == "strain_avg_5_microstrain_avg"  ~ "strain_5",
                                                       strains == "strain_avg_6_microstrain_avg"  ~ "strain_6",
                                                       strains == "strain_avg_7_microstrain_avg"  ~ "strain_7",
                                                       strains == "strain_avg_8_microstrain_avg"  ~ "strain_8",


                                                       TRUE ~ strains)) %>%

        # Filter data based on the show_strain parameter
        dplyr::filter(strain_number %in% show_strain) %>% {

        # Create plot
        ggplot2::ggplot(data = ., ggplot2::aes(x = date_time,
                                               y = strain_weight,
                                               colour = strain_number)) +

        # Show tare_count_smp as label if show_tare_group is TRUE
        {if(show_tare_group == TRUE)ggplot2::geom_text(ggplot2::aes(label = tare_count_smp))} +

        ggplot2::geom_point() +

        # Increase the number of axis ticks according to time_breaks
        ggplot2::scale_x_datetime(breaks = scales::date_breaks({{time_breaks}})) +

        # Add line with the mean value of the weights
        ggplot2::stat_smooth(ggplot2::aes(colour = strain_number),
                             se = FALSE) +

        # Choose the theme
        ggplot2::theme_bw() +

        # Set y-scales independent. I am leaving this line here for
        # remembering ggh4x library. It is useful!
        #ggh4x::facet_grid2(. ~variable, scales = "free_y", independent = "y") +

        # Edit x and y labs
        ggplot2::ylab("Strain weight (g)") +
        ggplot2::xlab("Time") +

        # Set strain colors
        ggplot2::scale_color_manual(values = strain_colors) +

        # Add median and set temp to the title
        ggplot2::ggtitle(stringr::str_c("Set temperature: ", .$set_point_t_avg_avg,
                               " Median temperature: ", stats::median(.$tc_avg_deg_c_avg))) +

        # Edit legend
        ggplot2::theme(legend.position = "bottom",
                       strip.text.x =  ggplot2::element_text(size = 25),
                       axis.text.y   = ggplot2::element_text(size = 15),

                       # Rotating and spacing axis labels
                       axis.text.x   = ggplot2::element_text(angle = 90,
                                                             vjust = 0.5,
                                                             hjust = 1,
                                                             size = 8),

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
}

#' plot_arrhenius
#'
#' @param residual_conductance_data Dataframe obtained with the
#' `calculate_residual_conductance` function
#'
#' @return A ggplot2 object with the weight (grams) measured by each strain (4 in
#' total) inside the Droughtbox.
#' @examples
#'
#' @export


plot_arrhenius <- function(residual_conductance_data){

    # Validate input parameters ------------------------------------------------

    # Check that droughtbox_data is a dataframe
    base::stopifnot("residual_conductance_data should be a dataframe of type data.frame" = "data.frame" %in% base::class(residual_conductance_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("residual_conductance column missing in the dataframe" =  "residual_conductance" %in% base::colnames(residual_conductance_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("set_temperature column missing in the dataframe" =  "set_temperature" %in% base::colnames(residual_conductance_data))

    # Make sure that the data is in the dataframe
    base::stopifnot("spcode column missing in the dataframe" =  "spcode" %in% base::colnames(residual_conductance_data))

    # Arrhenius plot -----------------------------------------------------------

}
