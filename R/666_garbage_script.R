# library(dplyr)
data <- read_hie_droughtbox_data("inst/extdata/acacia_aneura_25c.dat")
data
plot_raw_strains_weights(data)


plot_raw_strains_weights(clean_droughtbox_dataset(data))

# filter_hie_droughtbox_data(droughtbox_data = data,
#                            from_start_date = NULL,
#                            to_end_date = NULL,
#                            from_start_time = "12:51:00",
#                            to_end_time = "12:52:00")
#
# # nrow(data)
#View(data)
#
# # Test
#
#
#
# # if (any(is.null(from_start_time) | is.null(to_end_time))){
# #     stop("from_start_time and to_end_time must be both specified or set both to NULL. Time parameters must have a HH:MM:SS format i.e. 13:53:00")
# # }
#
# # filter_hie_droughtbox_data(data, from_start_date = "2024-03-04" ,
# #                            to_end_date = "2024-03-04") %>%
# #     nrow()
#
#
# # filter_hie_droughtbox_data(data,
# #                            from_start_date = , to_end_date =
# #                                from_start_time = , to_end_time =,
# # )
#
# lubridate::ymd(NULL) == 0
# hms::parse_hms(NULL)
#
# param_1 = NULL
# param_2 = NULL
# param_3 = 1
# param_4 = 1
# hms::parse_hms("15:10")
# hms::parse_hms("15:10:00") < hms::parse_hms("15:11:00")
#
# #from_start_time,to_end_time
#
#
#
# checkmate::anyNaN(hms::parse_hms(c("15:10", "15:11")))
# is.na(lubridate::ymd("1991-10-19"))
#
# data %>%
#     filter(time %in% (hms::parse_hms("15:10:00"):hms::parse_hms("15:11:00")))



