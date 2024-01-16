setup <- function(reset = FALSE){
  if (identical(Sys.getenv("bannr_url_base"),"") | reset) {
    if (utils::menu(c("Set it now", "Don't set it now"), title = "No base URL has been saved. For `bannr` to work well, it should be saved as an environmental variable.") == 1) {
      Sys.setenv("bannr_url_base" = readline("What's the base URL?"))
    }
  }

  if (identical(Sys.getenv("bannr_term"),"") | reset) {
    if (utils::menu(c("Set it now", "Don't set it now"), title = "No term has been set yet. For `bannr` to work well, it should be saved as an environmental variable.") == 1) {
      Sys.setenv("bannr_term" = readline("What term is this?"))
    }
  }

  url_base <- Sys.getenv("bannr_url_base")
  this_term <- Sys.getenv("bannr_term")
  url_login <- paste0(url_base, "twbkwbis.P_WWWLogin")
  url_rosters <- paste0(url_base, "bzlkfcwl.P_FacClaListSum?term=", this_term)
  url_attendances <- paste0(url_base, "bzlkrost.P_ViewRoster?term=", this_term)

  if (identical(Sys.getenv("bannr_url_login"),"") | reset) {
    prompt_response <- utils::menu(c("Use recommended URL", "Set it to something else", "Don't set it now"), title = paste("No login URL has been set yet. For `bannr` to work well, it should be saved as an environmental variable. The recommended login URL is", url_login))
    if (prompt_response == 1) {
      Sys.setenv("bannr_url_login" = url_login)
    } else if (prompt_response == 2) {
      Sys.setenv("bannr_url_login" = readline("What's the login URL?"))
    }
  }

  if (identical(Sys.getenv("bannr_url_rosters"),"") | reset) {
    prompt_response <- utils::menu(c("Use recommended URL", "Set it to something else", "Don't set it now"), title = paste("No roster URL has been set yet. For `bannr` to work well, it should be saved as an environmental variable. The recommended roster URL is", url_rosters))
    if (prompt_response == 1) {
      Sys.setenv("bannr_url_rosters" = url_rosters)
    } else if (prompt_response == 2) {
      Sys.setenv("bannr_url_rosters" = readline("What's the roster URL?"))
    }
  }

  if (identical(Sys.getenv("bannr_url_attendances"),"") | reset) {
    prompt_response <- utils::menu(c("Use recommended URL", "Set it to something else", "Don't set it now"), title = paste("No attendance URL has been set yet. For `bannr` to work well, it should be saved as an environmental variable. The recommended attendance URL is", url_attendances))
    if (prompt_response == 1) {
      Sys.setenv("bannr_url_attendances" = url_attendances)
    } else if (prompt_response == 2) {
      Sys.setenv("bannr_url_attendances" = readline("What's the attendance URL?"))
    }
  }

  key_list <- keyring::key_list("RStudio Keyring Secrets")$username
  if (!"bannr_username" %in% key_list) {
    rstudioapi::askForSecret(
      name = "bannr_username",
      message = "Username:",
      title = "Banner Web login")
  }
  if (!"bannr_password" %in% key_list) {
    rstudioapi::askForSecret(
      name = "bannr_password",
      message = "Password:",
      title = "Banner Web login")
  }

  message("Setup complete!")
}

#' Log in to Banner
#'
#' `authorize()` uses stored values to authorize a browsing session in Banner.
#'
#' @section On first run:
#' The first time `authorize()` is run, it will request and store key values for logging into Banner, including the institutional URL, the current term, and others. The user will also be prompted to store a login username and password in the system keychain.
#' 
#' @section First-run details requested: 
#' Values requested at this stage include the following:
#' 
#' 1. The **base url** of a Banner instance, used when navigating by web browser
#' 2. The **current term** used internally by Banner. This might take the form of something like "202420" for the second semester of the 2023-2024 academic year.
#' 3. For the **login page**, any url details added to the base url. Once the base url has been entered, a value is suggested.
#' 4. For the *course rosters page*, any url details added to the base url. Once the base url has been entered, a value is suggested.
#' 5. For the *attendance rosters page*, any url details added to the base url. Once the base url has been entered, a value is suggested.
#' 6. The **username** used to log in to Banner.
#' 7. The **password** used to log in to Banner.
#' 
#' @section Security considerations: 
#' The first five values are not sensitive, but the last two certainly are. The first five are stored as environmental variables. Meanwhile, the **username** and **password** are treated with heightened security, manged using [rstudioapi::askForSecret()] and, potentially, stored by it in the system keyring.
#'
#' @param reset Indicates previous values should be ignored and overwritten.
#'
#' @returns An `rvest` session
#' @export
#'
#' @examples
#' \dontrun{
#'   my_session <- authorize()
#' }
authorize <- function(reset = FALSE){
  check_setup("bannr_url_login", reset)

  key_list <- keyring::key_list("RStudio Keyring Secrets")$username
  if ("bannr_username" %in% key_list) {
    id <- keyring::key_get("RStudio Keyring Secrets", "bannr_username")
  } else {
    id <-
      rstudioapi::askForSecret(
      name = "bannr_username",
      message = "Username:",
      title = "Banner Web login")
  }

  if ("bannr_password" %in% key_list) {
    pw <- keyring::key_get("RStudio Keyring Secrets", "bannr_password")
  } else {
    pw <-
      rstudioapi::askForSecret(
      name = "bannr_password",
      message = "Password:",
      title = "Banner Web login")
  }

  my_session <- rvest::session(Sys.getenv("bannr_url_login"))
  my_form <- rvest::html_form(my_session)[[1]]
  filled_form <-
    rvest::html_form_set(
      my_form,
      sid = id,
      PIN = pw)

  bannr_session <- rvest::session_submit(my_session, filled_form)

  return(bannr_session)
}

check_login <- function(.session){
  is_login_page <- .session |>
    rvest::read_html() |>
    rvest::html_text() |>
    stringr::str_detect("^User Login")

  if (is_login_page) {
    stop("You've been logged out.\nUse `authorize()` and try again.", call. = FALSE)
  }
}

check_setup <- function(detail = "bannr_url_login", reset = FALSE){
  if (identical(Sys.getenv(detail),"")) {
    message("The `bannr` package hasn't yet been set up.\nPlease answer the following questions to speed up future uses of the `authorize()` function.")
    setup()
  } else if (reset) {
    message("Reset requested.\nPlease answer the following questions to speed up future uses of the `authorize()` function.")
    setup(reset)
  }
}
