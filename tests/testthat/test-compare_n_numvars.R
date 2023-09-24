# # User wants to compare at least 3 dependent variables 
# # against an independent variable that is:
# 
# # - Gaussian (indep_var=drive ratio)
# out1 <- compare_n_numvars(
#   .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
#   indep_var = "cyl",
#   gaussian = TRUE
# )
# saveRDS(out1, file = 'tests/testthat/n_numvars_out1.RDA')
# 
# # - Ordinal (indep_var= 4, 6 or 8 cylinders)
# out2 <- compare_n_numvars(
#   .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
#   indep_var = "cyl",
#   gaussian = FALSE
# )
# 
# saveRDS(out2, file = 'tests/testthat/n_numvars_out2.RDA')

# tests only work in build menu?
test_that("compare_n_numvars() with defaults and options set", { 
  # using @abusjahn's template written for meansd
  expected <- readRDS('n_numvars_out1.rda')
  expect_equal(compare_n_numvars(
    .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
    indep_var = "cyl",
    gaussian = TRUE
  )$raw, expected$raw)
  expected <- readRDS('n_numvars_out2.rda')
  expect_equal(compare_n_numvars(
    .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
    indep_var = "cyl",
    gaussian = FALSE)$results, 
  expected$results)
  # expect_equal(compare_n_numvars(
  #   .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
  #   indep_var = "cyl",
  #   gaussian = FALSE)$raw, 
  #   expected[[2]]$raw,
  #   ignore_attr=TRUE)
  # Ideas for testing for errors:

  # -Supply ordinal categories as a dep_var
  expect_error(compare_n_numvars(
    .data = diamonds, dep_vars = c("cut", "y", "z"),
    indep_var = "x",
    gaussian = FALSE
  ))
#  # -Should get one when: not enough groups.
#  #  this call failed at "expect_error()"
  
  # expect_error(compare_n_numvars(
  #   .data = diamonds, dep_vars = c("z"),
  #   indep_var = "x",
  #   gaussian = TRUE
  # ))
  
  # # -Supply ordinal categories to gaussian=T call
  # #  Also failed at expect_error()
  # expect_error(compare_n_numvars(
  #   .data = diamonds, dep_vars = c("x", "y", "z"),
  #   indep_var = "cut",
  #   gaussian = TRUE
  # ))
})

