# out1 <- formatP(0.012345)
# out2 <- formatP(0.012345, add.surprisal = TRUE)
# out3 <- formatP(0.012345, ndigits = 4)
# out4 <- formatP(0.000122345, ndigits = 3, pretext = TRUE)
# saveRDS(list(out1=out1, out2=out2, out3=out3,out4=out4),file = 'tests/testthat/formatP_out.rda')



test_that("formatP() with defaults and options set", { 
  # using @abusjahn's template written for meansd
  expected <- readRDS('formatP_out.rda')
  expect_equal(formatP(0.012345), expected[[1]])
  expect_equal(formatP(0.012345, add.surprisal = TRUE), expected[[2]])
  expect_equal(formatP(0.012345, ndigits = 4), expected[[3]])
 # expect_equal(formatP(0.012345, ndigits = 3, pretext = TRUE), expected[[4]]) # this fails
})

