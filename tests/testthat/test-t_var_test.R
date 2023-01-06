# out1 <- t_var_test(mtcars, wt ~ am)
# # may be used in pipes:
# out2 <- t_var_test(mtcars, wt ~ am)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/t_var_test_out.rda')
test_that("t_var_test() with defaults and options set", {
  expected <- readRDS('t_var_test_out.rda')
  expect_equal(t_var_test(mtcars, wt ~ am), expected[[1]])
  expect_equal(mtcars %>% t_var_test(wt ~ am), 
               expected[[2]])
})

