# out1 <- ksnormal(x = mtcars$wt,lillie=TRUE)
# out2 <- suppressWarnings(ks.test(
#   x = mtcars$wt, pnorm, mean = mean(mtcars$wt, na.rm = TRUE),
#   sd = sd(mtcars$wt, na.rm = TRUE))$p.value
# )
# names(out2) <- "p_Normal_KS"
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/ksnormal_out.rda')

test_that("ksnormal() without Lilliefors", {
  expected <- readRDS('ksnormal_out.rda')
  expect_equal(ksnormal(x = mtcars$wt,lillie = TRUE), expected[[1]])
  expect_equal(ksnormal(x = mtcars$wt, lillie = FALSE),expected[[2]])
})

