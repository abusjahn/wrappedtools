# out1 <- FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars))
# out2 <- FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars), exclude = "r")
# rawdata <- mtcars
# out3 <- FindVars(varnames = c("^c", "g"))
# saveRDS(list(out1=out1, out2=out2, out3=out3),file = 'tests/testthat/FindVars_out.rda')

test_that("FindVars() with defaults and options set", {
  expected <- readRDS('FindVars_out.rda')
  expect_equal(FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars)), expected[[1]])
  expect_equal(FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars), exclude = "r"), 
               expected[[2]])
# I don't think you can test whether naming an object after an argument causes
  # successful run of the function
  
  #  rawdata <-mtcars
<<<<<<< HEAD
#  expect_equal(FindVars(varnames = c("^c", "g")), expected[[3]])
=======
 expect_error(FindVars(varnames = c("^c", "g")))
>>>>>>> 4bbdaaa5d2d419dd8c40941370fb6047a322783f
})

