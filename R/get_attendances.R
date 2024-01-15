#' Get attendance data
#'
#' @param rosters A list of course data. Alternatively, an active `rvest` session.
#' @param .session (optional) An active `rvest` session. If this value is undefined, the session will be derived from an attribute set on `rosters`
#'
#' @returns A list of items, with one item per course. Each item will identify the course CRN, catalog number, title, schedule, a table showing the roster of student enrollments, and student attendance records. The current `rvest` session is attached as an attribute, used for chaining functions together.
#' @export
#'
#' @examples
#' \dontrun{
#'   my_classes <- authorize() |>
#'     get_rosters() |>
#'     get_attendances()
#' }
get_attendances <- function(rosters, .session = NULL) {
  if (rvest::is.session(rosters)) {
    rosters <- get_rosters(rosters)
  }
  if (is.null(.session)) {
    .session <- attributes(rosters)$bannr_session
  }
  crns <- names(rosters) |>
    stringr::str_remove_all("crn")
  attendance_rosters <- rosters
  for (crn in crns) {
    course_path <- Sys.getenv("bannr_url_attendances") |>
      paste0("&crn=", crn)

    attendance_page <- .session |>
      rvest::session_jump_to(course_path)

    check_login(attendance_page)

    attendance <-
      rvest::html_table(attendance_page, header = TRUE)[[7]]

    if (nrow(attendance) > 0) {
      this_crn <- paste0("crn", crn)

      attendance_rosters[[this_crn]]$attendance <- attendance |>
        tibble::tibble()
    }
  }

  attributes(attendance_rosters)$bannr_session <- attendance_page
  return(attendance_rosters)
}

#' Add summaries of attendance
#'
#' @param attendance_rosters A table of course enrollments and attendances
#' @param include_fridays Whether to count Fridays among a course's viable days of attendance. This parameter is useful when teaching an online or hybrid class when Fridays serve as marker of that week's attendance, but where Friday is otherwise missing from course records as the day of meeting.
#'
#' @returns A table of course enrollments and attendances, with added columns showing each student's current rate of attendance and the last date attended. The current `rvest` session is attached as an attribute, used for chaining functions together.
#' @export
#'
#' @examples
#' \dontrun{
#'   my_classes <- authorize() |>
#'     get_rosters() |>
#'     get_attendances() |>
#'     flatten_rosters() |>
#'     process_attendances()
#' }
process_attendances <- function(
    attendance_rosters, include_fridays = TRUE){
  if ("list" %in% class(attendance_rosters)) {
    attendance_rosters <- attendance_rosters |>
      flatten_rosters()
  } else if (rvest::is.session(attendance_rosters)) {
    attendance_rosters <- attendance_rosters |>
      get_attendances() |>
      flatten_rosters()
  }

  clean_days <- function(x, fridays) {
    x <- x |>
      stringr::str_split("") |>
      unlist() |>
      stringr::str_replace("M", "Mon") |>
      stringr::str_replace("T", "Tue") |>
      stringr::str_replace("W", "Wed") |>
      stringr::str_replace("R", "Thu") |>
      stringr::str_replace("F", "Fri")

    if (fridays) x <- unique(c(x, "Fri"))

    paste(x, collapse = ", ")
  }
  processed_df <- attendance_rosters |>
    dplyr::select(-c(catalog, start:end, paid:major)) |>
    tidyr::pivot_longer(cols = -c(crn:id),
                 names_to = "date") |>
    dplyr::group_by(crn, id) |>
    dplyr::mutate(
      days = clean_days(days, include_fridays),
      date = lubridate::mdy(date),
      day = lubridate::wday(date, label = TRUE),
      attend_rate = sum(value == "Y", na.rm = TRUE)/(sum(value == "Y", na.rm = TRUE) + sum(value == "N", na.rm = TRUE)),
      attend_last = dplyr::case_when(value == "Y" ~ date)) |>
    dplyr::filter(stringr::str_detect(days, as.character(day))) |>
    dplyr::mutate(
      attend_last = suppressWarnings(
        max(attend_last, na.rm = TRUE))) |>
    dplyr::ungroup() |>
    dplyr::mutate(date = format(date, "%a-%h-%d")) |>
    dplyr::select(-c(day, days)) |>
    tidyr::pivot_wider(names_from = date) |>
    dplyr::mutate(
      rate = dplyr::case_when(!is.nan(rate) ~ rate),
      attend_last = dplyr::case_when(!is.infinite(attend_last) ~ attend_last))

  processed <-
    attendance_rosters |>
    dplyr::select(crn:major) |>
    dplyr::left_join(processed_df,
              by = c("crn", "course", "name", "id")) |>
    dplyr::arrange(crn, name)

  attributes(processed)$bannr_session <- attributes(attendance_rosters)$bannr_session

  return(processed)
}
