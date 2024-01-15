
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bannr

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
# install.packages("devtools")
devtools::install_github("jmclawson/bannr")
```

## Using the package

After installation, key values are stored using the `setup()` function:

``` r
library(bannr)
setup()
```

After setup, use package functions to access needed information.

``` r
current_session <- authorize()

eng375_attendance <- current_session |> 
  get_attendances() |>
  process_attendances() |> 
  filter(course == "Literary Text Mining") |> 
  select(name, id, attend_rate, attend_last)

all_seniors <- current_session |> 
  get_rosters() |> 
  flatten_rosters() |> 
  filter(classification == "Senior") |> 
  select(course, name, major)
```
