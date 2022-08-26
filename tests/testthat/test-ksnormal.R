# out1 <- ksnormal(x = mtcars$wt)
<<<<<<< HEAD
# out2 <- ks.test(
#   x = mtcars$wt, pnorm, mean = mean(mtcars$wt, na.rm = TRUE),
#   sd = sd(mtcars$wt, na.rm = TRUE)
# )
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/ksnormal_out.rda')

test_that("ksnormal() with defaults and options set", {
  expected <- readRDS('ksnormal_out.rda')
  expect_equal(ksnormal(x = mtcars$wt), expected[[1]])
  expect_equal(ks.test(
    x = mtcars$wt, pnorm, mean = mean(mtcars$wt, na.rm = TRUE),
    sd = sd(mtcars$wt, na.rm = TRUE)#, 
   # exact = FALSE #tried this to get rid of warning
  ), 
  expected[[2]])
=======
# out2 <- suppressWarnings(ks.test(
#   x = mtcars$wt, pnorm, mean = mean(mtcars$wt, na.rm = TRUE),
#   sd = sd(mtcars$wt, na.rm = TRUE))$p.value
# )
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/ksnormal_out.rda')

test_that("ksnormal() with defaults", {
  expected <- readRDS('ksnormal_out.rda')
  expect_equal(ksnormal(x = mtcars$wt), expected[[1]])
  expect_equal(ksnormal(x = mtcars$wt),expected[[2]])
>>>>>>> 4bbdaaa5d2d419dd8c40941370fb6047a322783f
})

