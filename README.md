
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bannr <a href="https://jmclawson.github.io/bannr/"><img src="man/figures/logo.png" align="right" height="139" alt="bannr website" /></a>

<!-- badges: start -->
<!-- badges: end -->

Using rvest and tidyverse tools, bannr helps instructors get clean,
actionable data on their courses, their students, and other information
that can otherwise only be retrieved via a web browser. From logging in
to exporting rosters and attendance records, bannr’s goal is to make it
easy for instructors to access course data in clean digital formats.

## Installation

The latest version of bannr can be installed from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jmclawson/bannr")
```

## Using the package

After installation, load the package with `library()`. Begin a session
with `authorize()` and go from there.

``` r
library(bannr)
my_courses <- authorize() |> 
  get_course_table()
```
