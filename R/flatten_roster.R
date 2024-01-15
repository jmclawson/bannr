#' Convert list of course data to a table
#'
#' @param rosters A list of course data
#'
#' @returns A table of course data. The current `rvest` session is attached as an attribute, used for chaining functions together.
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
flatten_rosters <- function(rosters) {
  output <- rosters |>
    purrr::map(flatten_roster) |>
    dplyr::bind_rows()

  attributes(output)$bannr_session <- attributes(rosters)$bannr_session

  return(output)
}

flatten_roster <- function(roster) {
  this_roster <- roster$roster |>
    dplyr::mutate(
      crn = roster$crn,
      catalog = roster$catalog,
      course = roster$title,
      days = roster$schedule[1],
      start = roster$schedule[2],
      end = roster$schedule[3],
      .before = name)

  if (nrow(roster$attendance) > 0) {
    attendance <- roster$attendance |>
      tidyr::separate_wider_regex(
        NAMEID,
        c(name = ".*", id = "G\\d+?"))

    this_roster <- this_roster |>
      dplyr::left_join(attendance,
                by = c("name", "id"))
  }
  return(this_roster)
}
