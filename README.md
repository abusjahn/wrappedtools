
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wrappedtools

<!-- badges: start -->
<!-- badges: end -->

The goal of wrappedtools is to …

## Installation

You can install the released version of wrappedtools from github with:

``` r
devtools::install_github("abusjahn/wrappedtools")
```

## Example

This is a basic example which shows you how to solve a common problem,
that is, describe and test differences in some measures between 2
samples, rounding descriptive statistics to a reasonable precision in
the process:

``` r
library(wrappedtools)
#> Lade nötiges Paket: rlang
#> 
#> Attache Paket: 'rlang'
#> The following object is masked from 'package:crayon':
#> 
#>     chr
#> Lade nötiges Paket: tidyverse
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.1.1     v dplyr   1.0.5
#> v tidyr   1.1.3     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x purrr::%@%()         masks rlang::%@%()
#> x ggplot2::%+%()       masks crayon::%+%()
#> x purrr::as_function() masks rlang::as_function()
#> x rlang::chr()         masks crayon::chr()
#> x dplyr::filter()      masks stats::filter()
#> x purrr::flatten()     masks rlang::flatten()
#> x purrr::flatten_chr() masks rlang::flatten_chr()
#> x purrr::flatten_dbl() masks rlang::flatten_dbl()
#> x purrr::flatten_int() masks rlang::flatten_int()
#> x purrr::flatten_lgl() masks rlang::flatten_lgl()
#> x purrr::flatten_raw() masks rlang::flatten_raw()
#> x purrr::invoke()      masks rlang::invoke()
#> x dplyr::lag()         masks stats::lag()
#> x purrr::list_along()  masks rlang::list_along()
#> x purrr::modify()      masks rlang::modify()
#> x purrr::prepend()     masks rlang::prepend()
#> x purrr::splice()      masks rlang::splice()
#> Lade nötiges Paket: magrittr
#> 
#> Attache Paket: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> The following object is masked from 'package:rlang':
#> 
#>     set_names
#> Package wrappedtools is still experimental, be warned that there might be dragons
compare2numvars(data = mtcars, testvars = c('wt','mpg', "disp"), 
                groupvar = 'am',
                gaussian = F,
                round_desc = 3)
#> # A tibble: 3 x 5
#> # Groups:   Variable [3]
#>   Variable desc_all         `am 0`           `am 1`           p    
#>   <fct>    <glue>           <chr>            <chr>            <chr>
#> 1 wt       3.32 (2.53/3.66) 3.52 (3.44/3.84) 2.32 (1.90/2.81) 0.001
#> 2 mpg      19.2 (15.3/22.8) 17.3 (14.8/19.2) 22.8 (20.6/30.4) 0.002
#> 3 disp     196 (121/337)    276 (177/360)    120 (79/160)     0.001
```
