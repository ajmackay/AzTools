# AzTools

<!-- badges: start -->

<!-- badges: end -->

The goal of AzTools is to ...

## Installation

You can install the development version of AzTools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ajmackay/AzTools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AzTools)
## basic example code
```

## Building the Package

1.  Make Changes
2.  usethis::use_package("any new dependencies")
3.  devtools::document()
4.  load
5.  devtools::check()
6.  usethis::version

## Function Ideas

Function to post issues to github

<code>post.issue \<- function(title, body) { gh::gh(endpoint = "POST /repos/ajmackay/AzTools/issues", title = title, body = body) }</code>

-   Add dummy data
