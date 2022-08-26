# Duplicate. Rewrite the file with a different "out2" assignment if 
# you would like to use the placeholder

# out1 <- pairwise_t_test(dep_var = mtcars$wt, indep_var = mtcars$cyl)
# out2 <- pairwise_t_test(dep_var = mtcars$wt, indep_var = mtcars$cyl)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/pairwise_t_test_out.rda')
test_that("pairwise_t_test() with defaults and options set", {
  expected <- readRDS('pairwise_t_test_out.rda')
  expect_equal(pairwise_t_test(dep_var = mtcars$wt, indep_var = mtcars$cyl), expected[[1]])
  expect_equal(pairwise_t_test(dep_var = mtcars$wt, indep_var = mtcars$cyl), 
               expected[[2]])
})

