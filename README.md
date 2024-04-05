
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HIEdroughtbox

<!-- badges: start -->
<!-- badges: end -->

The goal of HIEdroughtbox is to facilitate the analysis of the data
coming from the [droughtbox located at the Hawkesbury Institute for the
Environment](https://ecamo19.github.io/droughtbox_documentation/)

## Installation

You can install the development version of HIEdroughtbox from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ecamo19/hie_dRoughtbox")
```

## Example

``` r
library(HIEdroughtbox)
library(dplyr)
```

### Read droughtbox data

``` r
data <- read_hie_droughtbox_data("inst/extdata/acacia_aneura_25c.dat")
head(data)
  tare_count_smp           date_time       date     time air_tc_avg_deg_c_avg
1             13 2024-03-04 12:51:00 2024-03-04 12:51:00                26.76
2             13 2024-03-04 12:51:30 2024-03-04 12:51:30                26.80
3             13 2024-03-04 12:52:20 2024-03-04 12:52:20                26.83
4             13 2024-03-04 12:52:20 2024-03-04 12:52:20                26.83
5             14 2024-03-04 13:01:10 2024-03-04 13:01:10                27.22
6             14 2024-03-04 13:01:10 2024-03-04 13:01:10                27.22
  rh_avg_percent_avg tc_avg_deg_c_avg set_point_t_avg_avg set_point_vpd_avg_avg
1              44.05            27.19                  25                   1.6
2              43.63            27.25                  25                   1.6
3              43.36            27.29                  25                   1.6
4              43.36            27.29                  25                   1.6
5              39.73            27.79                  25                   1.6
6              39.73            27.79                  25                   1.6
  set_point_abs_h_avg_avg vpd_avg_kpa_avg abs_h_avg_g_m3_avg
1                   11.39           1.967              11.19
2                   11.39           1.986              11.10
3                   11.39           1.999              11.05
4                   11.39           1.999              11.05
5                   11.39           2.177              10.35
6                   11.39           2.177              10.35
  set_point_rh_avg_avg strain_avg_1_microstrain_avg
1                49.48                     2.472753
2                49.48                     2.489199
3                49.48                     2.505648
4                49.48                     2.505648
5                49.48                     2.485881
6                49.48                     2.485881
  strain_avg_2_microstrain_avg strain_avg_3_microstrain_avg
1                     2.434764                     2.381188
2                     2.451971                     2.349167
3                     2.417776                     2.333139
4                     2.417776                     2.333139
5                     2.410692                     2.406784
6                     2.410692                     2.406784
  strain_avg_4_microstrain_avg
1                     2.310796
2                     2.310783
3                     2.327533
4                     2.327533
5                     2.287311
6                     2.287311
```

### Visualize climatic conditions

``` r
plot_droughtbox_climatic_controls(data, cowplot = T)
```

<img src="man/figures/README-example_plot_climatic_controls-1.png" width="100%" />

### Visualize raw weights measured by the strains

``` r
plot_strains_weights(data, show_strain = "all", 
                     time_breaks = "5 min", 
                     show_tare_group = TRUE)
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="man/figures/README-example_plot_raw_weights-1.png" width="100%" />
\#### Visualize some strains

``` r
plot_strains_weights(data, 
                     show_strain = c("strain_1", "strain_4"), 
                     time_breaks = "10 min",
                     show_tare_group = TRUE)
Warning in if (show_strain == "all") {: the condition has length > 1 and only
the first element will be used
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="man/figures/README-example_plot_raw_weights_1_4-1.png" width="100%" />

### Choose a range of data

``` r
filtered_data <- filter_droughtbox_data(data, 
                       from_start_time = "13:10:00",
                       to_end_time = "15:10:00")
[1] "Times must have a HH:MM:SS format i.e. 13:53:00"
[1] "Dates must have a YYYY-MM-DD format i.e. 1991-10-19"
[1] "Filtering data by hour from: 13:10:00 to: 15:10:00"

head(filtered_data)
  tare_count_smp           date_time       date     time air_tc_avg_deg_c_avg
1             15 2024-03-04 13:10:40 2024-03-04 13:10:40                27.66
2             15 2024-03-04 13:11:00 2024-03-04 13:11:00                27.67
3             15 2024-03-04 13:11:10 2024-03-04 13:11:10                27.68
4             15 2024-03-04 13:11:20 2024-03-04 13:11:20                27.69
5             15 2024-03-04 13:11:30 2024-03-04 13:11:30                27.70
6             15 2024-03-04 13:11:40 2024-03-04 13:11:40                27.70
  rh_avg_percent_avg tc_avg_deg_c_avg set_point_t_avg_avg set_point_vpd_avg_avg
1              35.75            28.30                  25                   1.6
2              35.61            28.26                  25                   1.6
3              35.49            28.32                  25                   1.6
4              35.48            28.29                  25                   1.6
5              35.45            28.32                  25                   1.6
6              35.24            28.31                  25                   1.6
  set_point_abs_h_avg_avg vpd_avg_kpa_avg abs_h_avg_g_m3_avg
1                   11.39           2.381               9.54
2                   11.39           2.387               9.51
3                   11.39           2.394               9.48
4                   11.39           2.394               9.49
5                   11.39           2.397               9.48
6                   11.39           2.405               9.43
  set_point_rh_avg_avg strain_avg_1_microstrain_avg
1                49.48                     2.443138
2                49.48                     2.443120
3                49.48                     2.459561
4                49.48                     2.443120
5                49.48                     2.410238
6                49.48                     2.426678
  strain_avg_2_microstrain_avg strain_avg_3_microstrain_avg
1                     2.465538                     2.365185
2                     2.448162                     2.381247
3                     2.465310                     2.349241
4                     2.448166                     2.365244
5                     2.448166                     2.365244
6                     2.465310                     2.365244
  strain_avg_4_microstrain_avg
1                     2.240364
2                     2.240412
3                     2.240412
4                     2.240412
5                     2.290720
6                     2.206874
```

``` r
plot_strains_weights(filtered_data, 
                     show_strain = c("strain_1", "strain_4"), 
                     time_breaks = "10 min",
                     show_tare_group = TRUE)
Warning in if (show_strain == "all") {: the condition has length > 1 and only
the first element will be used
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="man/figures/README-example_plot_weights_1_4_filtered_data-1.png" width="100%" />

### Clean data

``` r
clean_data <- 
    clean_droughtbox_data(filtered_data, 
                      remove_n_observations = 10, 
                      threshold = 0.2)
[1] "Total number of rows removed: 251"

head(clean_data)
# A tibble: 6 × 17
# Groups:   tare_count_smp [1]
  tare_count_smp date_time           date       time     air_tc_avg_deg_c_avg
  <fct>          <dttm>              <date>     <time>                  <dbl>
1 15             2024-03-04 13:12:30 2024-03-04 13:12:30                 27.7
2 15             2024-03-04 13:12:40 2024-03-04 13:12:40                 27.8
3 15             2024-03-04 13:12:50 2024-03-04 13:12:50                 27.8
4 15             2024-03-04 13:13:00 2024-03-04 13:13:00                 27.8
5 15             2024-03-04 13:13:10 2024-03-04 13:13:10                 27.8
6 15             2024-03-04 13:13:20 2024-03-04 13:13:20                 27.8
# ℹ 12 more variables: rh_avg_percent_avg <dbl>, tc_avg_deg_c_avg <dbl>,
#   set_point_t_avg_avg <dbl>, set_point_vpd_avg_avg <dbl>,
#   set_point_abs_h_avg_avg <dbl>, vpd_avg_kpa_avg <dbl>,
#   abs_h_avg_g_m3_avg <dbl>, set_point_rh_avg_avg <dbl>,
#   strain_avg_1_microstrain_avg <dbl>, strain_avg_2_microstrain_avg <dbl>,
#   strain_avg_3_microstrain_avg <dbl>, strain_avg_4_microstrain_avg <dbl>
```

``` r
plot_strains_weights(clean_data, 
                     show_strain = "all",
                     show_tare_group = FALSE)
`geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="man/figures/README-example_plot_weights_all_cleaned_data-1.png" width="100%" />
