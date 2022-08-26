# mtcars2 <- mutate(mtcars, cyl = factor(cyl, ordered = TRUE))
# out1 <- pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear)
# # Only comparison against reference gear=3
# out2 <- pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear, ref = TRUE)
# saveRDS(list(out1=out1, out2=out2, mtcars2=mtcars2),file = 'tests/testthat/pairwise_ordcat_testout.rda')


test_that("pairwise_ordcat_test() with defaults and options set", {
  expected <- readRDS('pairwise_ordcat_testout.rda')
  mtcars2 <- mutate(mtcars, cyl = factor(cyl, ordered = TRUE))
  expect_equal(pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear), expected[[1]])
  expect_equal(pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear, ref = TRUE), 
               expected[[2]])
})

