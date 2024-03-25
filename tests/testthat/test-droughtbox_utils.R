# Search globally the .dat files
#Sys.glob("assets/*.dat")

# Test for clean_droughtbox_colnames -------------------------------------------

test_that("Return object of type character for clean_droughtbox_colnames", {
  expect_type(clean_droughtbox_colnames(path_data_droughtbox = "assets/droughtbox_output.dat"), "character")
})

test_that("Return object of lenght 30 for clean_droughtbox_colnames", {
    expect_equal(length(clean_droughtbox_colnames(path_data_droughtbox = "assets/droughtbox_output.dat")), 30)
})



# Test for read_hie_droughtbox_data --------------------------------------------


