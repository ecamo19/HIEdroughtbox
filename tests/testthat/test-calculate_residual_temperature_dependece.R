# Test for calculate_residual_temperature_dependence ---------------------------
test_tp_data <- read.csv("assets/tp_test_data.csv", header = TRUE) %>%
    dplyr::mutate(gmin = as.numeric(g_min))



test_that("Test that calculate_residual_temperature_dependence return 6 rows", {
    expect_equal(nrow(calculate_residual_temperature_dependence(gmin = test_tp_data$gmin,
                                                               temperature = test_tp_data$temperature)),
                 6)
})

test_that("Test that calculate_residual_temperature_dependence return 9 columns", {
    expect_equal(ncol(calculate_residual_temperature_dependence(gmin = test_tp_data$gmin,
                                                                temperature = test_tp_data$temperature)),
                 9)
})
