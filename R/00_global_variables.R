#' @description
#' This script is meant to solve the 'no visible binding for global variable'
#' warning when check() is run.
#'
#' @noRd
#'
#' @keywords internal

utils::globalVariables(c('time', 'vpd_avg_kpa_avg', "set_point_t_avg_avg", '.',
                         'set_temperature', 'strain_avg_1_microstrain_avg',
                         'strain_avg_2_microstrain_avg',
                         'strain_avg_3_microstrain_avg',
                         'strain_avg_4_microstrain_avg', 'strain_number',
                         'slope_grams_per_second', 'spcode', 'tree_id',
                         'transpiration_grams_per_sec_cm2', 'median_vpd',
                         'tare_count_smp', 'comparison', 'date_time',
                         'set_point_vpd_avg_avg', 'tc_avg_deg_c_avg',
                         'air_tc_avg_deg_c_avg', 'rh_avg_percent_avg',
                         'set_point_rh_avg_avg', 'abs_h_avg_g_m3_avg',
                         'set_point_abs_h_avg_avg', 'enough_observations',
                         'strains', 'strain_weight', 'timestamp_ts', 'record_rn',
                         'p_output_avg_avg', 'd_output_avg_avg', 'i_avg_avg',
                         'batt_v_min_volts_min', 'i_output_avg_avg',
                         'duty_cycle_avg_avg', 'vr1000_avg_1_mv_v_avg',
                         'vr1000_avg_2_mv_v_avg', 'vr1000_avg_3_mv_v_avg',
                         'vr1000_avg_4_mv_v_avg', 't_sg_avg_1_avg',
                         't_sg_avg_2_avg', 't_sg_avg_3_avg', 't_sg_avg_4_avg',
                         'branch_basal_diameter_mm', 'branch_length_cm',
                         'leaf_area_cm2', 'surface_branch_area_cm2',
                         'branch_basal_radius_cm', 'data', 'n', 'notes',
                         "number_of_values", "estimate", "coef"

                         ))
