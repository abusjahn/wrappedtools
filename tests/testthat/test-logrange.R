# I'm not sure it's possible to do the same "saveRDS - readRDS" kind of methodology with ggplot2 objects
# because I'm not too clear on compressing to or decompressing from binary files


# out1 <- ggplot(mtcars, aes(wt, mpg)) +
#   geom_point() +
#   scale_y_log10(breaks = logrange_5)
# out2 <- ggplot(mtcars, aes(wt, mpg)) +
#   geom_point() +
#   scale_y_log10(breaks = logrange_123456789)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/logrange_1_out.rda')
test_that("logranges have expected length", {
  # expected <- readRDS('logrange_1_out.rda')
  expect_equal(length(logrange_1), 41)
  expect_equal(length(logrange_15), 82)
  expect_equal(length(logrange_12357), 205)
  expect_equal(length(logrange_123456789), 369)
  expect_equal(length(logrange_5), 738)
})

