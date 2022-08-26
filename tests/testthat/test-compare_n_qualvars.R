# # Separate lines for each factor level:
# out1 <- compare_n_qualvars(
#   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#   spacer = " "
# )
# # All levels in one row but with linebreaks:
# out2 <- compare_n_qualvars(
#   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#   singleline = TRUE)
# # All levels in one row, separated by ";":
# out3 <- compare_n_qualvars(
#   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#   singleline = TRUE, linebreak = "; ")
# saveRDS(list(out1=out1, out2=out2, out3=out3),file = 'tests/testthat/compare_n_qualvars_out.rda')
# 
# test_that("compare_n_qualvars() with defaults and options set", {
#   expected <- readRDS('compare_n_qualvars_out.rda')
#   expect_equal(compare_n_qualvars(
#     data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#     spacer = " "),
#     expected[[1]])
#   expect_equal(compare_n_qualvars(
#     data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#     singleline = TRUE),
#     expected[[2]])
#   expect_equal(compare_n_qualvars(
#     data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#     singleline = TRUE, linebreak = "; "),
#     expected[[3]])
# })


