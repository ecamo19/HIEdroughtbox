
# Tests for clean_droughtbox_colnames() ----------------------------------------

test_that("Return object of type character for clean_droughtbox_colnames", {
  expect_type(clean_droughtbox_colnames("assets/droughtbox_output.dat"), "character")
})

test_that("Return object of length 30 for clean_droughtbox_colnames", {
    expect_equal(length(clean_droughtbox_colnames("assets/droughtbox_output.dat")),

                 # Number of colnames expected
                 30)
})

# Tests for read_hie_droughtbox_data_file() -----------------------------------------
test_that("Return object of type data.frame", {
    expect_equal(class(read_hie_droughtbox_data_file("assets/droughtbox_output.dat")),

                 # class expected
                 "data.frame")
})

test_that("Return object of lenght 25 for clean_droughtbox_colnames", {
    expect_equal(length(read_hie_droughtbox_data_file("assets/droughtbox_output.dat")),

                 # Number of columns expected
                 17)
})

# Tests for create_empty_droughtbox_leaf_branch_areas_sheet() ------------------

# Missing tests #

# Tests for read_hie_droughtbox_leaf_branch_areas() ----------------------------
test_that("Return object of type data.frame", {
    expect_equal(class(read_hie_droughtbox_leaf_branch_areas("assets/input_branch_length_diameter.csv")),

                 # class expected
                 "data.frame")
})

test_that("Return columns with no NAs when branch diameter and lenght are provided", {

    # Read data
    data <- read_hie_droughtbox_leaf_branch_areas("assets/input_branch_length_diameter.csv") %>%

        # Get columns
        dplyr::select(surface_branch_area_cm2, leaf_area_cm2)

    # No NAs in columns test.
    expect_contains(c(FALSE, FALSE), sapply(data, function(x) any(is.na(x))))

})

test_that("Return columns with no NAs when branch diameter and lenght are NOT provided", {

    # Read data
    data <- read_hie_droughtbox_leaf_branch_areas("assets/input_leaf_branch_areas.csv") %>%

        # Get columns
        dplyr::select(surface_branch_area_cm2, leaf_area_cm2)

    # No NAs in columns test.
    expect_contains(c(FALSE, FALSE), sapply(data, function(x) any(is.na(x))))

})

# Tests for filter_droughtbox_data() -------------------------------------------
test_data <- read_hie_droughtbox_data_file("assets/droughtbox_output.dat")

test_that("Return an error when all parameters are set to NULL",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = NULL ,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = NULL))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = NULL ,
                                            to_end_date = NULL,
                                            from_start_time = "12:51:00",
                                            to_end_time = NULL))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = NULL,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = "12:51:00"))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = "2024-04-03" ,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = NULL))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = NULL,
                                            to_end_date = "2024-04-03",
                                            from_start_time = NULL,
                                            to_end_time = NULL))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = "2024-04-03" ,
                                            to_end_date = NULL,
                                            from_start_time = "12:51:00",
                                            to_end_time = NULL))
})

test_that("Return an error when one parameter is specified without the other",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = "2024-04-03" ,
                                            to_end_date = NULL,
                                            from_start_time = "12:51:00",
                                            to_end_time = NULL))
})

test_that("Return an error if time parameter is specified without secods",{
    expect_error(filter_droughtbox_data(droughtbox_data = test_data,
                                            from_start_date = NULL,
                                            to_end_date = NULL,
                                            from_start_time = "12:51",
                                            to_end_time = "12:52"))
})

test_that("Return 2 rows out of 796",{
          expect_equal(nrow(filter_droughtbox_data(droughtbox_data = test_data,
                                                  from_start_date = NULL,
                                                  to_end_date = NULL,
                                                  from_start_time = "12:51:00",
                                                  to_end_time = "12:52:00")), 2)
})

test_that("Return 796 rows out of 796",{
    expect_equal(nrow(filter_droughtbox_data(droughtbox_data = test_data,
                                                 from_start_date = "2024/03/04",
                                                 to_end_date = "2024/03/04",
                                                 from_start_time = NULL,
                                                 to_end_time = NULL)), 796)
})

test_that("Return 2 rows out of 796",{
    expect_equal(nrow(filter_droughtbox_data(droughtbox_data = test_data,
                                                 from_start_date = "2024/03/04",
                                                 to_end_date = "2024/03/04",
                                                 from_start_time = "12:51:00",
                                                 to_end_time = "12:52:00")), 2)
})

# filter_droughtbox_data(data, from_start_date = "2024/03/",
# to_end_date = "2024/03/")

# Test for clean_droughtbox_data() ---------------------------------------------
test_data <- read_hie_droughtbox_data_file("assets/droughtbox_output.dat")

test_that("Return 5 row from a dataset with 13 rows",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:23,],
                                               remove_n_observations = 5)), 5)

})

test_that("Return 5 rows from a dataset with 14 rows",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:24,],
                                               remove_n_observations = 5)), 5)

})

test_that("Return 5 rows from a dataset with 15 rows. remove_n_observations = 6 ",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:25,],
                                               remove_n_observations = 6)), 5)

})

test_that("Return 5 rows from a dataset with 15 rows.  remove_n_observations = 5",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:25,],
                                               remove_n_observations = 5)), 5)

})

test_that("Return 5 rows from a dataset with 50 rows",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:60,],
                                               remove_n_observations = 10)), 5)

})

test_that("Return 10 rows from a dataset with two tarecounts",{
    expect_equal(nrow(clean_droughtbox_data(test_data[11:110,],
                                               remove_n_observations = 10)), 10)

})

test_that("Return error when tare groups don't have enough tares",{
    expect_error(nrow(clean_droughtbox_data(test_data[1:13,],
                                               remove_n_observations = 6)))

})

# Tests for merge_droughtbox_data() --------------------------------------------

test_that("Return error when only a dataframe is specified",{

    test_data_1 <- read_hie_droughtbox_data_file('assets//acacia_aneura_25c.dat')

    expect_error(merge_droughtbox_data(test_data_1))
})

test_that("Return dataframe with 16 rows",{

    test_data_1 <- read_hie_droughtbox_data_file('assets//acacia_aneura_25c.dat')

    test_data_2 <- read_hie_droughtbox_data_file('assets//acacia_aneura_30c.dat')

    expect_equal(nrow(merge_droughtbox_data(test_data_1[1:3,],
                                            test_data_2[1:3,],
                                            test_data_1[31:40,])), 16)
})



