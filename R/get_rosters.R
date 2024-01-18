#' Get roster data
#'
#' `get_rosters()` uses an active `rvest` session to find data on each course, including a table of students enrolled.
#'
#' @param courses A course table
#' @param .session (optional) An active `rvest` session. If this value is undefined, the session will be derived from an attribute set on `courses`
#' @param term (optional) The term to be used in this query. If undefined, this value will be inherited from `rosters` or from the environmental variable defined at setup.
#'
#' @returns A list of items, with one item per course. Each item will identify the course CRN, catalog number, title, schedule, and a table showing the roster of student enrollments. The current `rvest` session is attached as an attribute, used for chaining functions together.
#' @export
#'
#' @examples
#' \dontrun{
#'   my_classes <- authorize() |>
#'     get_rosters()
#' }
get_rosters <- function(courses, .session = NULL, term = NULL) {
  if (rvest::is.session(courses)) {
    courses <- get_course_table(courses, term)
  }
  if (is.null(.session)) {
    .session <- attributes(courses)$bannr_session
  }
  if (is.null(term)) {
    term <- attributes(courses)$term
  }
  rosters <- list()
  for (i in 1:nrow(courses)) {
    crn <- courses$crn[i]
    catalog <- paste(courses$subj[i],
                     courses$num[i],
                     courses$sec[i])
    title <- courses$title[i]
  
  bannr_url_rosters <- Sys.getenv("bannr_url_rosters") |> 
    stringr::str_replace_all(Sys.getenv("bannr_term"), term)

    course_path <- bannr_url_rosters |>
      paste("&crn=",
            crn,
            sep = "")
    crn_page <- .session |>
      rvest::session_jump_to(course_path)

    check_login(crn_page)

    this_roster <-
      rvest::html_table(
        crn_page,
        header = TRUE)[[7]]

    this_roster <- this_roster |>
      dplyr::select(-c(1,7,8,11))

    colnames(this_roster) <-
      c("name", "id", "paid",
        "graduating", "dropped",
        "classification", "major")

    this_roster <- this_roster |>
      dplyr::mutate(
        dropped = !stringr::str_detect(dropped, "Web"))

    this_sched <-
      rvest::html_table(
        crn_page,
        header = TRUE)[[5]][2,2] |># switched from magrittr
      as.character()

    this_crn <- paste0("crn", crn)

    rosters[[this_crn]]$crn <- crn
    rosters[[this_crn]]$catalog <- catalog
    rosters[[this_crn]]$title <- title
    rosters[[this_crn]]$schedule <- c(
      days = this_sched |>
        as.character() |>
        strsplit("   ") |>
        unlist() |>
        {\(x) x[1]}(),
      start = this_sched |>
        as.character() |>
        strsplit("   ") |>
        unlist() |>
        {\(x) x[2]}() |>
        strsplit(" - ") |>
        unlist() |>
        {\(x) x[1]}(),
      end = this_sched |>
        as.character() |>
        strsplit("   ") |>
        unlist() |>
        {\(x) x[2]}() |>
        strsplit(" - ") |>
        unlist() |>
        {\(x) x[2]}())
    rosters[[this_crn]]$roster <- this_roster
  }

  attributes(rosters)$bannr_session <- crn_page
  attributes(rosters)$term <- term
  return(rosters)
}
