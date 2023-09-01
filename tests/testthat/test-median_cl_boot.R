# #2 calls to median_cl_boot
# out1 <- median_cl_boot(x = mtcars$wt, nrepl = 10^5)
# out2 <- median_cl_boot(x = mtcars$wt, conf=0.99,, nrepl = 10^5)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/median_cl_bootout.rda')


test_that("median_cl_boot() with defaults and options", {
  expected <- readRDS('median_cl_bootout.rda')
  expect_equal(round(median_cl_boot(mtcars$wt, nrepl = 10^5),2), 
                     round(expected[[1]],2),
               tolerance = 1)
  expect_equal(round(median_cl_boot(x = mtcars$wt, conf = 0.99, nrepl = 10^5),
                     2),
               round(expected[[2]],2),
               tolerance = 1)
})

