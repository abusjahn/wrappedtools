
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wrappedtools

<!-- badges: start -->
<!-- badges: end -->

The goal of ‘wrappedtools’ is to make my (and possibly your) life a bit
easier by a set of convenience functions for many common tasks like
e.g. computation of mean and SD and pasting them with ±. Instead of  
paste(round(mean(x),some\_level), round(sd(x),some\_level), sep=‘±’)  
a simple meansd(x, roundDig = some\_level) is enough.

## Installation

You can install the released version of ‘wrappedtools’ from github with:

``` r
devtools::install_github("abusjahn/wrappedtools")
```

## Examples

This is a basic example which shows you how to solve a common problem,
that is, describe and test differences in some measures between 2
samples, rounding descriptive statistics to a reasonable precision in
the process:

``` r
# Standard functions to obtain median and quartiles:
median(mtcars$mpg)
#> [1] 19.2
quantile(mtcars$mpg,probs = c(.25,.75))
#>    25%    75% 
#> 15.425 22.800
# wrappedtools adds rounding and pasting:
median_quart(mtcars$mpg)
#> [1] "19 (15/23)"
# on a higher level, this logic leads to
compare2numvars(data = mtcars, dep_vars = c('wt','mpg', "disp"), 
                indep_var = 'am',
                gaussian = FALSE,
                round_desc = 3)
#> # A tibble: 3 x 5
#> # Groups:   Variable [3]
#>   Variable desc_all         `am 0`           `am 1`           p    
#>   <fct>    <chr>            <chr>            <chr>            <chr>
#> 1 wt       3.32 (2.53/3.66) 3.52 (3.44/3.84) 2.32 (1.90/2.81) 0.001
#> 2 mpg      19.2 (15.3/22.8) 17.3 (14.8/19.2) 22.8 (20.6/30.4) 0.002
#> 3 disp     196 (121/337)    276 (177/360)    120 (79/160)     0.001
```

To explain the \*wrapper’ part of the package name, here is another
example, using the ks.test as test for a Normal distribution, where
ksnormal simply wraps around the ks.test function:

``` r
somedata <- rnorm(100)
ks.test(x = somedata, 'pnorm', mean=mean(somedata), sd=sd(somedata))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  somedata
#> D = 0.057193, p-value = 0.8991
#> alternative hypothesis: two-sided

ksnormal(somedata)
#> [1] 0.899124
```

Saving variable selections: Variables may fall into different groups:
Some are following a Gaussian distribution, others are ordinal or
factorial. There may be several grouping variables like treatment,
gender… To refer to such variables, it is convenient to have their index
and name stored. The name may be needed as character or , symbol,
complex variable names like “size \[cm\]” may need to be surrounded by
backtics in some function calls but must not have those in others.  
Function FindVars finds columns in tibbles or dataframes, based on name
pattern. This is comparable to the selection helpers in ‘tidyselect’,
but does not select the content of matching variables, but names,
positions, and count:

``` r
gaussvars <- FindVars(varnames = c('wt','mpg'),
                      allnames = colnames(mtcars))
gaussvars
#> $index
#> [1] 1 6
#> 
#> $names
#> [1] "mpg" "wt" 
#> 
#> $bticked
#> [1] "`mpg`" "`wt`" 
#> 
#> $symbols
#> $symbols[[1]]
#> mpg
#> 
#> $symbols[[2]]
#> wt
#> 
#> 
#> $count
#> [1] 2

#Exclusion based on pattern
factorvars <- FindVars(varnames = c('a','cy'),
                      allnames = colnames(mtcars),
                      exclude = c('t'))
factorvars$names
#> [1] "cyl"  "am"   "gear" "carb"
```

This should give you the general idea, I’ll try to expand this intro
over time…
