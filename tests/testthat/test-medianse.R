# #2 calls to meanse
# out1 <- meanse(x = mtcars$mpg)
# out2 <- meanse(x = mtcars$mpg, roundDig = 5)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/meanseout.rda')
test_that("meanse() with defaults and options set", {
  expected <- readRDS('meanseout.rda')
  expect_equal(meanse(mtcars$mpg), expected[[1]])
  expect_equal(meanse(x = mtcars$mpg, roundDig = 5), 
               expected[[2]])
})

