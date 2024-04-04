
# Tests for plot_droughtbox_climatic_controls ----------------------------------
test_that("Return object of type gg or ggplot", {
    data <- read_hie_droughtbox_data("assets/droughtbox_output.dat")
    expect_contains(class(plot_raw_strains_weights(data)), "ggplot")
})


# Tests for plot_raw_strains_weights -------------------------------------------
test_that("Return object of type gg or ggplot", {
    data <- read_hie_droughtbox_data("assets/droughtbox_output.dat")
    expect_contains(class(plot_raw_strains_weights(data)), "ggplot")
})

