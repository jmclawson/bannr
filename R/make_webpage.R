#' Create webpage for attendance taking
#'
#' `make_attendance_dashboard()` makes a clean webpage to speed the process of taking attendance at the start of class. While Banner requires loading five webpages to change tabs, choose the link for attendance, and select the term, course, and date, `bannr`'s attendance dashboard links directly to the final step. Links to course rosters are also provided.
#'
#' @param df A table of course data, such as the one derived after [flatten_rosters()]. This table needs to have columns that include `crn`, `catalog`, `course`, `days`, `start`, and `end`.
#' @param filename The path and file name for saving the HTML file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   authorize() |> 
#'     get_rosters() |>
#'     flatten_rosters() |>
#'     make_attendance_dashboard("take-attendance.html")
#' }
make_attendance_dashboard <- function(df, filename = "attendance-dashboard.html"){
  clean_hour <- function(time) {
    if (is.na(time)) {
      return(as.integer(time))
    } else {
      hour <- stringr::str_extract(time, "^[0-9]+:") |> 
        stringr::str_remove_all(":") |> 
        as.integer()
    }
    
    if (hour == 12) {
      if (stringr::str_detect(tolower(time), "am")) {
        hour <- 0
      } else {
        hour <- 12
      }
    } else if (stringr::str_detect(tolower(time), "pm")) {
      hour <- hour + 12
    }
    return(hour)
  }
  
  get_term <- function(){
    the_term <- Sys.getenv("bannr_term")
    
    if(nchar(Sys.getenv("bannr_term")) == 6){
      the_year <- Sys.getenv("bannr_term") |> 
        substr(0,4)
      the_semester <- Sys.getenv("bannr_term") |> 
        substr(5,6)
      if (the_semester == "10") {
        the_semester <- "Fall"
      } else if (the_semester == "20") {
        the_semester <- "Spring"
      } else if (the_semester == "30") {
        the_semester <- "Summer I"
      } else if (the_semester == "40") {
        the_semester <- "Summer II"
      }
      the_term <- paste(the_semester, the_year)
    }
    
    return(the_term)
  }
  
  course_summary <- df |> 
    dplyr::select(crn, catalog, course, days, start, end) |> 
    dplyr::distinct() |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      catalog = catalog |> 
        stringr::str_replace_all("([0-9]{3}) ([0-9]+$)",
                                 "\\1-\\2"),
      start = toupper(start),
      end = toupper(end),
      start_hr = clean_hour(start),
      day = dplyr::case_when(
        days == "MWF" ~ 1,
        days == "MW" ~ 2,
        days == "TR" ~ 3,
        TRUE ~ 4)) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(desc(substr(days,1,1)), start_hr) |> 
    dplyr::mutate(order = dplyr::row_number()) |> 
    dplyr::mutate(
      name = paste0(
        "<span class='class-time'>",
        stringr::str_remove_all(start, ":00"),
        "</span>",
        "<span class='class-catalog'>",
        catalog,
        "</span>",
        "<span class='class-title'>",
        course,
        "</span>",
        "</span>") |> 
        stringr::str_remove_all("<span class='class-time'>NA</span>"),
      start = dplyr::if_else(is.na(start), "-", start),
      end = dplyr::if_else(is.na(end), "-", end)
    )
  
  course_summary <<- course_summary
  
  var_starts <- paste0(
    "\n\tvar classStart = [dateObj('",
    paste0(course_summary$start,
           collapse = "'), dateObj('"),
    "')];")
  var_ends <- paste0(
    "\tvar classEnd = [dateObj('",
    paste0(course_summary$end,
           collapse = "'), dateObj('"),
    "')];")
  var_day <- paste0(
    "\tvar classDay = [",
    paste0(course_summary$day,
           collapse = ", "),
    "];")
  var_crn <- paste0(
    '\tvar classCRN = ["',
    paste0(course_summary$crn,
           collapse = '", "'),
    '"];')
  var_name <- paste0(
    '\tvar className = ["',
    paste0(course_summary$name,
           collapse = '", "'),
    '"];')
  var_url_attend <- paste0(
    '\tvar attendURL = "',
    Sys.getenv("bannr_url_attendance_take"),
    '";')
  var_url_roster <- paste0(
    '\tvar rosterURL = "',
    Sys.getenv("bannr_url_rosters"),
    '";')
  js_array <- paste0('\tfor (i in [',
                     paste0((1:nrow(course_summary))-1,
                            collapse=','),
                     ']) {')
  var_cssIDs <- paste0('var cssIDs = [',
                       paste0('"Class',
                              1:nrow(course_summary),
                              collapse = '",'),
                       '"];\n',
                       js_array)
  
  div_mwf_header <- course_summary |> 
    dplyr::pull(day)
  if (1 %in% div_mwf_header) {
    div_mwf_header <- "<h2>Mon / Wed / Fri</h2>\n"
  } else {
    div_mwf_header <- "<h2>Mon / Wed</h2>\n"
  }
  div_mwf <- div_mwf_header |> 
    paste0('<ul>\n',
           '<li><a href="#" id="',
           paste0(course_summary |> 
                    dplyr::filter(day %in% 1:2) |> 
                    dplyr::pull(order) |> 
                    {\(x) paste0("Class", x)}(),
                  collapse = '"></a></li>\n<li><a href="#" id="'),
           '"></a></li>\n',
           '</ul>')
  
  div_tr <- "<h2>Tues / Thur </h2>\n" |> 
    paste0('<ul>\n',
           '<li><a href="#" id="',
           paste0(course_summary |> 
                    dplyr::filter(day == 3) |> 
                    dplyr::pull(order) |> 
                    {\(x) paste0("Class", x)}(),
                  collapse = '"></a></li>\n<li><a href="#" id="'),
           '"></a></li>\n',
           '</ul>')
  
  div_online <- "<h2>Online</h2>\n" |> 
    paste0('<ul>\n',
           '<li><a href="#" id="',
           paste0(course_summary |> 
                    dplyr::filter(day == 4) |> 
                    dplyr::pull(order) |> 
                    {\(x) paste0("Class", x)}(),
                  collapse = '"></a></li>\n<li><a href="#" id="'),
           '"></a></li>\n',
           '</ul>\n')
  
  if (sum(course_summary$day %in% 1:2) == 0) div_mwf <- ""
  if (sum(course_summary$day == 3) == 0) div_tr <- ""
  if (sum(course_summary$day == 4) == 0) div_online <- ""
  div_all <- paste(div_mwf, div_tr, div_online, sep = "\n")
  var_all <- paste(var_starts, var_ends, var_day, var_crn, var_name, var_url_attend, var_url_roster, sep = "\n")
  pg_pt_1 <- "attendance_page_pt1.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  pg_pt_2 <- "attendance_page_pt2.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  pg_pt_3 <- "attendance_page_pt3.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  pg_pt_4 <- "attendance_page_pt4.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  pg_pt_5 <- "attendance_page_pt5.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  pg_pt_6 <- "attendance_page_pt6.html" |> 
    system.file(package = "bannr") |> 
    readLines() |> 
    paste0(collapse="\n")
  webpage <- paste0(
    pg_pt_1,
    paste0('<h1 class="">Attendance, ',
           get_term(),
       '</h1>'),
    pg_pt_2,
    div_all,
    pg_pt_3,
    var_all,
    pg_pt_4, "\n",
    var_cssIDs,"\n",
    pg_pt_5,
    js_array,"\n",
    pg_pt_6
    )
  
  webpage |> 
    cat(file = filename)
}

