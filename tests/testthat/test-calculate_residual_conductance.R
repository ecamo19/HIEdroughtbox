# Tests for calculate_residual_conductance -------------------------------------

# test_that("Return a dataframe with 7 columns in residual conductance", {
#     droughtbox_data <- read_hie_droughtbox_data_file("assets/droughtbox_output.dat")
#     species_areas <- readxl::read_excel("assets/input_leaf_branch_areas.xlsx") %>%
#                         dplyr::select(-notes)
#
#     residual_conductance_data <- calculate_residual_conductance(droughtbox_data = droughtbox_data,
#                                    leaf_and_branch_area_data = species_areas)
#
#
#     expect_equal(colnames(residual_conductance_data),
#                  c("spcode","tree_id", "strain_number",
#                    "set_temperature", "transpiration_grams_per_sec_cm2",
#                    "median_vpd", "residual_conductance"))
# })
# Tests the maximun length of a data-frame is 8 per temperature treatment -------
