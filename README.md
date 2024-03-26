
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HIEdroughtbox

<!-- badges: start -->
<!-- badges: end -->

The goal of HIEdroughtbox is to …

## Installation

You can install the development version of HIEdroughtbox from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ecamo19/hie_dRoughtbox")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(HIEdroughtbox)
library(dplyr)
```

### Read droughtbox data

``` r
data <- read_hie_droughtbox_data("data/acacia_aneura_25c.dat")

head(data)
            date_time       date     time air_tc_avg_deg_c_avg
1 2024-03-04 12:51:00 2024-03-04 12:51:00                26.76
2 2024-03-04 12:51:30 2024-03-04 12:51:30                26.80
3 2024-03-04 12:52:20 2024-03-04 12:52:20                26.83
4 2024-03-04 12:52:20 2024-03-04 12:52:20                26.83
5 2024-03-04 13:01:10 2024-03-04 13:01:10                27.22
6 2024-03-04 13:01:10 2024-03-04 13:01:10                27.22
  rh_avg_percent_avg tc_avg_deg_c_avg t_sg_avg_1_avg t_sg_avg_2_avg
1              44.05            27.19          23.32          23.38
2              43.63            27.25          23.31          23.39
3              43.36            27.29          23.30          23.37
4              43.36            27.29          23.30          23.37
5              39.73            27.79          23.22          23.32
6              39.73            27.79          23.22          23.32
  t_sg_avg_3_avg t_sg_avg_4_avg set_point_t_avg_avg set_point_vpd_avg_avg
1          23.52          23.36                  25                   1.6
2          23.52          23.42                  25                   1.6
3          23.51          23.35                  25                   1.6
4          23.51          23.35                  25                   1.6
5          23.50          23.29                  25                   1.6
6          23.50          23.29                  25                   1.6
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
  strain_avg_4_microstrain_avg vr1000_avg_1_mv_v_avg vr1000_avg_2_mv_v_avg
1                     2.310796           0.005057652            0.06213687
2                     2.310783           0.005040844            0.06212001
3                     2.327533           0.005024034            0.06215351
4                     2.327533           0.005024034            0.06215351
5                     2.287311           0.005108020            0.06252284
6                     2.287311           0.005108020            0.06252284
  vr1000_avg_3_mv_v_avg vr1000_avg_4_mv_v_avg tare_count_smp
1           -0.01737413           -0.01322383             13
2           -0.01734051           -0.01322381             13
3           -0.01732367           -0.01324060             13
4           -0.01732367           -0.01324060             13
5           -0.01728998           -0.01347576             14
6           -0.01728998           -0.01347576             14
```

### Visualize climatic conditions

``` r
plot_droughtbox_climatic_controls(data, cowplot = T)
```

<img src="man/figures/README-example_plot_climatic_controls-1.png" width="100%" />

### Visualize **raw** weights measured by the strains

``` r
plot_raw_strains_weights(data)
```

<img src="man/figures/README-example_plot_raw_weights-1.png" width="100%" />
