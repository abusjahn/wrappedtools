# 
# out1 <- cortestR(mtcars[, c("wt", "mpg", "qsec")], split = FALSE, sign_symbol = TRUE)
# # separate coefficients and p-values
# out2 <- cortestR(mtcars[, c("wt", "mpg", "qsec")], split = TRUE, sign_symbol = FALSE)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/cortestR_out.rda')
test_that("cortestR() with defaults and options set", {
  expected <- readRDS('cortestR_out.rda')
  expect_equal(cortestR(mtcars[, c("wt", "mpg", "qsec")], split = FALSE, sign_symbol = TRUE), expected[[1]])
  expect_equal(cortestR(mtcars[, c("wt", "mpg", "qsec")], split = TRUE, sign_symbol = FALSE), 
               expected[[2]])
})

