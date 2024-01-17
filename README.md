
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bannr <a href="https://jmclawson.github.io/bannr/"><img src="man/figures/logo.png" align="right" height="139" alt="bannr website" /></a>

<!-- badges: start -->
<!-- badges: end -->

Using rvest and tidyverse tools, bannr helps course instructors get
clean, actionable data on their courses, their students, and other
information that can otherwise only be navigated via a web interface in
the browser. From logging in to exporting attendance records, bannr’s
goal is to make it easy for instructors to use course data without
needing to enter things by hand.

## Installation

You can install the development version of bannr from
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
all_data <- authorize() |> 
  process_attendances()
```
