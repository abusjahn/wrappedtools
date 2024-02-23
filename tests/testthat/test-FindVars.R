# out1 <- FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars))
# out2 <- FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars),
#                  exclude = "r", return_symbols = TRUE)
# #' rawdata <- mtcars
# # out3 <- FindVars(varnames = c("^c", "g"))
# saveRDS(list(out1=out1, out2=out2),file = 'tests/testthat/FindVars_out.rda')

test_that("FindVars() with defaults and options set", {
  expected <- readRDS('FindVars_out.rda')
  expect_equal(FindVars(varnames = c("^c", "g"), 
                        allnames = colnames(mtcars)), 
               expected[[1]])
  expect_equal(FindVars(varnames = c("^c", "g"), 
                        allnames = colnames(mtcars), 
                        exclude = "r",
                        return_symbols = TRUE), 
               expected[[2]])
# I don't think you can test whether naming an object after an argument causes
  # successful run of the function
  
  #  rawdata <-mtcars
 expect_error(FindVars(varnames = c("^c", "g")))
})

