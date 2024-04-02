# Search globally the .dat files
#Sys.glob("assets/*.dat")
library(dplyr)
library(stringr)

# Tests for clean_droughtbox_colnames ------------------------------------------

test_that("Return object of type character for clean_droughtbox_colnames", {
  expect_type(clean_droughtbox_colnames("assets/droughtbox_output.dat"), "character")
})

test_that("Return object of length 30 for clean_droughtbox_colnames", {
    expect_equal(length(clean_droughtbox_colnames("assets/droughtbox_output.dat")), 30)
})


# Tests for read_hie_droughtbox_data -------------------------------------------
test_that("Return object of type data.frame", {
    expect_equal(class(read_hie_droughtbox_data("assets/droughtbox_output.dat")), "data.frame")
})

test_that("Return object of lenght 25 for clean_droughtbox_colnames", {
    expect_equal(length(read_hie_droughtbox_data("assets/droughtbox_output.dat")), 17)
})
