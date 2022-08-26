# out1 <- median_quart(x = mtcars$mpg)
# out2 <- median_quart(x = mtcars$mpg, roundDig = 5, groupvar = mtcars$cyl,
#                range = TRUE, add_n = TRUE, .german = TRUE)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/medianquart.rda')
test_that("median_quart() with defaults and options set", {
  expected <- readRDS('medianquart.rda')
  expect_equal(median_quart(mtcars$mpg), expected[[1]])
  expect_equal(median_quart(x = mtcars$mpg, roundDig = 5, groupvar = mtcars$cyl, 
                      range = TRUE, add_n = TRUE, .german = TRUE), 
               expected[[2]])
})


