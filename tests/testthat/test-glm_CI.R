# 
# out1 <- glm_out <- glm(am ~ mpg, family = binomial, data = mtcars)
# out2 <- glmCI(glm_out)
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/glmCI_out.rda')

test_that("glmCI() with defaults and options set", {
  expected <- readRDS('glmCI_out.rda')
  expect_equal(glm(am ~ mpg, family = binomial, data = mtcars), expected[[1]])

})

