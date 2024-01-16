# # # Whole process
# # #
#
# bannr_session <- authorize()
#
# course_table <- bannr_session |>
#   get_course_table()
#
# rosters <- course_table |>
#   get_rosters()
#
# attendance <- rosters |>
#   get_attendances()
#
# bannr_data <- attendance |>
#   flatten_rosters()
#
# banner_data <- authorize() |>
#   get_course_table() |>
#   get_rosters() |>
#   get_attendances() |>
#   flatten_rosters() |>
#   process_attendances()

get_course_records <- function() {
  authorize() |>
    get_course_table() |>
    get_rosters() |>
    get_attendances() |>
    flatten_rosters() |>
    process_attendances()
}

# processed <- get_course_records()

# banner_data <- get_course_records()
