
# out1 <- meansd(x = mtcars$mpg)
# out2 <- meansd(x = mtcars$mpg, roundDig = 5, groupvar = mtcars$cyl,
#                range = TRUE, add_n = TRUE, .german = TRUE)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/meansdout.rda')
test_that("meansd() with defaults and options set", {
  expected <- readRDS('meansdout.rda')
  expect_equal(meansd(mtcars$mpg), expected[[1]])
  expect_equal(meansd(x = mtcars$mpg, roundDig = 5, groupvar = mtcars$cyl, 
                      range = TRUE, add_n = TRUE, .german = TRUE), 
               expected[[2]])
})

