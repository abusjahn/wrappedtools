# out1 <- cat_desc_stats(mtcars$gear)
# out2 <- cat_desc_stats(mtcars$gear, return_level = FALSE)
# out3 <- cat_desc_stats(mtcars$gear, groupvar = mtcars$am)
# out4 <- cat_desc_stats(mtcars$gear, groupvar = mtcars$am, singleline = TRUE)
# saveRDS(list(out1=out1, out2=out2,out3=out3,out4=out4),file = 'tests/testthat/cat_desc_stats_out.rda')

test_that("cat_desc_stats() with defaults and options set", {
  expected <- readRDS('cat_desc_stats_out.rda')
  expect_equal(cat_desc_stats(mtcars$gear), expected[[1]])
  expect_equal(cat_desc_stats(mtcars$gear, return_level = FALSE), expected[[2]])
  expect_equal(cat_desc_stats(mtcars$gear, groupvar = mtcars$am), expected[[3]])
  expect_equal(cat_desc_stats(mtcars$gear, groupvar = mtcars$am, singleline = TRUE), expected[[4]])
})

