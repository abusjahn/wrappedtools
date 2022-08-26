# # Don't pull this if it's too simple
# out1 <- SEM(x = mtcars$wt)
# saveRDS(list(out1=out1),file = 'tests/testthat/SEM_out.rda')
test_that("SEM_() with defaults", {
  expected <- readRDS('SEM_out.rda')
  expect_equal(SEM(mtcars$wt), expected[[1]])
  
})

