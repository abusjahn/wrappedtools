# # All pairwise comparisons
# set.seed(1958)
# out1 <- pairwise_fisher_test(dep_var = mtcars$cyl, indep_var = mtcars$gear)
# # Only comparison against reference gear=3
# set.seed(1958)
# out2 <- pairwise_fisher_test(dep_var = mtcars$cyl, indep_var = mtcars$gear, ref = TRUE)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/pairwise_fisher_out.rda')


test_that("pairwise_fisher_test() with defaults and options set", {
  expected <- readRDS('pairwise_fisher_out.rda')
  set.seed(1958)
  expect_equal(pairwise_fisher_test(dep_var = mtcars$cyl, 
                                    indep_var = mtcars$gear), 
               expected[[1]],tolerance=1e-2)
  expect_equal(pairwise_fisher_test(dep_var = mtcars$cyl, 
                                    indep_var = mtcars$gear, 
                                    ref = TRUE), 
               expected[[2]],tolerance=1e-2)
})

