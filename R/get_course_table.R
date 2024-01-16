#' Get a table of courses
#'
#' @param .session An active `rvest` session
#'
#' @returns A tibble showing the CRN, title, subject, catalog number, and enrollment for each course. The current `rvest` session is attached as an attribute, used for chaining functions together.
#' @export
#'
#' @examples
#' \dontrun{
#'   my_courses <- authorize() |>
#'     get_course_table()
#' }
get_course_table <- function(.session){
  check_setup("bannr_url_rosters")

  page <- .session |>
    rvest::session_jump_to(Sys.getenv("bannr_url_rosters"))

  check_login(page)

  crns <- page |>
    rvest::html_nodes("option") |>
    rvest::html_attr("value")

  courses <- page |>
    rvest::html_nodes("option") |>
    rvest::html_text()

  course_table <-
    data.frame(crn = crns,
               course = courses) |>
    tidyr::separate_wider_delim(
      course,
      delim = ": ",
      names = c("course", "title")) |>
    tidyr::separate_wider_delim(
      course,
      delim = " ",
      names = c("subj", "num", "sec")) |>
    tidyr::separate_wider_delim(
      title,
      delim = ", ",
      names = c("title", "other")) |>
    tidyr::separate_wider_delim(
      other,
      delim = " ",
      names = c("drop", "enrolled")) |>
    dplyr::mutate(enrolled = enrolled |>
             stringr::str_extract("[0-9]+")) |>
    dplyr::mutate(dplyr::across(.cols = c("num",
                            "sec",
                            "enrolled"),
                  as.integer)) |>
    dplyr::select(-drop) |>
    tibble::tibble()

  attributes(course_table)$bannr_session <- page
  course_table
}

