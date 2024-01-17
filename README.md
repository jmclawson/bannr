
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

After installation, load the package with `library()`:

``` r
library(bannr)
```

On first use, the `authorize()` function will request and store some key
values before logging in to Banner.

``` r
current_session <- authorize()
```

Once authorized, package functions combine well with common methods for
working with data.

``` r
library(dplyr)
eng200_attendance <- current_session |> 
  get_attendances() |>
  process_attendances() |> 
  filter(course == "World Literature") |> 
  arrange(attend_rate)

all_seniors <- current_session |> 
  get_rosters() |> 
  flatten_rosters() |> 
  filter(classification == "Senior") |> 
  select(course, name, major)
```

Although bannr is deliberately limited to functions for retrieving data,
thereby avoiding potential pitfalls of bad submissions, it can also help
with common tasks such as entering attendance at the beginning of class.
The `make_attendance_dashboard()` function prepares an HTML file with
direct links to take attendance for classes listed, with today’s date
filled out:

``` r
current_session |> 
  get_attendances() |>
  process_attendances() |> 
  make_attendance_dashboard("attendance-dashboard.html")
```

The resulting dashboard makes is easy to jump to the final step for
recording today’s class attendance—often necessary at universities with
high rates of financial aid.

<img src="../../../../private/var/folders/sy/0bs1ndc90816137f2p8zl_9m0000gn/T/Rtmp8JAJ0w/temp_libpath103f918729c32/bannr/attendance-dashboard.gif" width="100%" />
