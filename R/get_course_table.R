#' Get a table of courses
#'
#' @param .session An active `rvest` session
#' @param term (optional) The term to be used in this query. If undefined, this value will be inherited from the environmental variable defined at setup.
#'
#' @returns A tibble showing the CRN, title, subject, catalog number, and enrollment for each course. The current `rvest` session is attached as an attribute, used for chaining functions together.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'   my_courses <- authorize() |>
#'     get_course_table()
#' }
get_course_table <- function(.session, term = NULL){
  check_setup("bannr_url_rosters")
  
  bannr_url_rosters <- 
  if (is.null(term)) {
    term <- Sys.getenv("bannr_term")
  }
  
  bannr_url_rosters <- Sys.getenv("bannr_url_rosters") |> 
    stringr::str_replace_all(Sys.getenv("bannr_term"), term)

  page <- .session |>
    rvest::session_jump_to(bannr_url_rosters)

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
  attributes(course_table)$term <- term
  
  course_table
}

