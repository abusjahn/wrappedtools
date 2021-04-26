test_that("roundR() rounds flexible to a given precision", {
  expect_equal(roundR(1.234), '1.2')
  expect_equal(roundR(1.234,level = 4,.german = T), '1,234')
})
