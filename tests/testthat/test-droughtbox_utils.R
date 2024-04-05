
# Tests for clean_droughtbox_colnames ------------------------------------------

test_that("Return object of type character for clean_droughtbox_colnames", {
  expect_type(clean_droughtbox_colnames("assets/droughtbox_output.dat"), "character")
})

test_that("Return object of length 30 for clean_droughtbox_colnames", {
    expect_equal(length(clean_droughtbox_colnames("assets/droughtbox_output.dat")),

                 # Number of colnames expected
                 30)
})


# Tests for read_hie_droughtbox_data -------------------------------------------
test_that("Return object of type data.frame", {
    expect_equal(class(read_hie_droughtbox_data("assets/droughtbox_output.dat")),

                 # class expected
                 "data.frame")
})

test_that("Return object of lenght 25 for clean_droughtbox_colnames", {
    expect_equal(length(read_hie_droughtbox_data("assets/droughtbox_output.dat")),

                 # Number of columns expected
                 17)
})

# Tests for filter_droughtbox_data -----------------------------------------
test_data <- read_hie_droughtbox_data("assets/droughtbox_output.dat")

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

# filter_droughtbox_data(data, from_start_date = "2024/03/", to_end_date = "2024/03/")

# clean_droughtbox_dataset -----------------------------------------------------
test_data <- read_hie_droughtbox_data("assets/droughtbox_output.dat")


test_that("Return 1 row from a dataset with 13 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:23,],
                                               remove_n_observations = 6)), 1)

})

test_that("Return 2 rows from a dataset with 14 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:24,],
                                               remove_n_observations = 6)), 2)

})

test_that("Return 3 rows from a dataset with 15 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:25,],
                                               remove_n_observations = 6)), 3)

})

test_that("Return 5 rows from a dataset with 15 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:25,],
                                               remove_n_observations = 5)), 5)

})

test_that("Return 30 rows from a dataset with 50 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:60,],
                                               remove_n_observations = 10)), 30)

})



test_that("Return 60 rows from a dataset with 50 rows",{
    expect_equal(nrow(clean_droughtbox_dataset(test_data[11:110,],
                                               remove_n_observations = 10)), 59)

})



test_that("Return error when tare groups don't have enough tares",{
    expect_error(nrow(clean_droughtbox_dataset(test_data[1:13,],
                                               remove_n_observations = 6)))

})





