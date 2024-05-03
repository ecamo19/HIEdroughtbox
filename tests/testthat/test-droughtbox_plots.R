
# Tests for plot_droughtbox_climatic_controls ----------------------------------
test_that("Return object of type gg or ggplot", {
    data <- read_hie_droughtbox_data_file("assets/droughtbox_output.dat")
    expect_contains(class(plot_droughtbox_climatic_controls(data)), "ggplot")
})


# Tests for plot_raw_strains_weights -------------------------------------------
test_that("Return object of type gg or ggplot", {
    data <- read_hie_droughtbox_data_file("assets/droughtbox_output.dat")
    expect_contains(class(plot_strains_weights(data)), "ggplot")
})

