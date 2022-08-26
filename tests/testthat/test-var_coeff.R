# # Don't pull this if it's too simple
# out1 <- var_coeff(x = mtcars$wt)
# saveRDS(list(out1=out1),file = 'tests/testthat/var_coeff_out.rda')
test_that("var_coeff() with defaults and options set", {
  expected <- readRDS('var_coeff_out.rda')
  expect_equal(var_coeff(mtcars$wt), expected[[1]])
  
})

