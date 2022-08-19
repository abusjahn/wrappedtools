# Author: Bilal
# First time testing a more complex function in a programming package of any kind


# First we want to lay out the possible testable properties of this function output
# -list lengths match each other in testXcnn 
# -with a table element known through manual testing or ideally reliably auto-calculated
# -

# Errors that were thrown as I manually tested my work on the script:
#   -joined list dimensions in tibble
#   -rawdata items being named improperly

# This gets you a random test dataset out of the list in the line below
# Random dataset out of 3: all dataset[[1 to 4]] columns are numerical dep_vars, 
# dataset[[5]] is an ordinal indep_var
# 
RandomTestDatasetOrd <- rlist::list.sample(.data=
                                             list(
                                               tibble(mtcars$mpg,
                                                      mtcars$wt,
                                                      mtcars$hp,
                                                      mtcars$drat,
                                                      mtcars$cyl),
                                               tibble(iris),
                                               tibble(ggplot2::diamonds$carat,
                                                      ggplot2::diamonds$x, 
                                                      ggplot2::diamonds$y, 
                                                      ggplot2::diamonds$z, 
                                                      ggplot2::diamonds$cut)),
                                           size=1)

tibble_itself <- RandomTestDatasetOrd[[1]]

test_that("compare_n_numvars() creates the same lengths of lists when creating the raw table", {
  expect_equal(roundR(1.234), '1.2')
  expect_equal(roundR(1.234,level = 4,.german = T), '1,234')
})