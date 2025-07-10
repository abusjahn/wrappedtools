# # Assuming Normal distribution:
# out1 <- compare2numvars(
#   data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
#   gaussian = TRUE
# )
# # # Ordinal scale:
# out2 <- compare2numvars(
#   data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
#   gaussian = FALSE
# )
# out3 <- compare2numvars(
#   data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
#   gaussian = TRUE, singleline = FALSE, n_boot = 10^3, round_desc = 2
# ) |>
# dplyr::filter(!stringr::str_detect(Variable,  "CI"))
# saveRDS(list(out1 = out1, out2 = out2, out3 = out3), file = "tests/testthat/compare2numvars_out.rda")


test_that("compare2numvars() with defaults and options set, plus tests for errors", {
  expected <- readRDS("compare2numvars_out.rda")
  expect_equal(compare2numvars(
    data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
    gaussian = TRUE
  ), expected[[1]])
  expect_equal(
    compare2numvars(
      data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
      gaussian = FALSE
    ),
    expected[[2]]
  )
  expect_equal(
    compare2numvars(
      data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
      gaussian = TRUE, singleline = FALSE, n_boot = 10^3, round_desc = 2
    ) |>
      dplyr::filter(!stringr::str_detect(Variable, "CI")),
    expected[[3]]
  )
  expect_error(compare2numvars(
    data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = c("am", "cyl"),
    gaussian = FALSE
  ))
  expect_error(compare2numvars(
    data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = c("am", "cyl"),
    gaussian = FALSE
  ))
  expect_error(compare2numvars(
    data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = c("am", "cyl"),
    gaussian = TRUE
  ))
  expect_error(compare2numvars(
    data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = c("am", "cyl"),
    gaussian = TRUE
  ))
})
