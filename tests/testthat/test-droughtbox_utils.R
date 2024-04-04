# Search globally the .dat files
#Sys.glob("assets/*.dat")
library(dplyr)
library(stringr)

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

# Tests for filter_hie_droughtbox_data -----------------------------------------
data <- read_hie_droughtbox_data("assets/droughtbox_output.dat")

test_that("Return an error when all parameters are set to NULL"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = NULL ,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = NULL
                                            ))
}

test_that("Return an error when one parameter is specified without the other"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = NULL ,
                                            to_end_date = NULL,
                                            from_start_time = "12:51:00",
                                            to_end_time = NULL
    ))
}


test_that("Return an error when one parameter is specified without the other"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = NULL,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = "12:51:00"
    ))
}


test_that("Return an error when one parameter is specified without the other"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = "2024-04-03" ,
                                            to_end_date = NULL,
                                            from_start_time = NULL,
                                            to_end_time = NULL
                                            ))
}


test_that("Return an error when one parameter is specified without the other"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = NULL,
                                            to_end_date = "2024-04-03",
                                            from_start_time = NULL,
                                            to_end_time = NULL
    ))
}



test_that("Return an error when one parameter is specified without the other"){
    expect_error(filter_hie_droughtbox_data(data = data,
                                            from_start_date = "2024-04-03" ,
                                            to_end_date = NULL,
                                            from_start_time = "12:51:00",
                                            to_end_time = NULL
    ))
}


# filter_hie_droughtbox_data(data, from_start_time = "12:15:16" , to_end_time = NULL)
# filter_hie_droughtbox_data(data, from_start_time = NULL , to_end_time = "12:15:16")
#
# filter_hie_droughtbox_data(data, from_start_date = NULL , to_end_date = "2024-03-04")
# filter_hie_droughtbox_data(data, from_start_date = "2024-03-04", to_end_date = NULL)
#
# filter_hie_droughtbox_data(data, from_start_time = "12:15" , to_end_time = "12:15")
# filter_hie_droughtbox_data(data, from_start_date = "2024/03/", to_end_date = "2024/03/")
