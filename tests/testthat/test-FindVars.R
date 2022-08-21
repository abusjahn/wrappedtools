#Brief desc:
#FindVars finds column names by searching for character argument within strings
#that make up the column names

#test for a known length of colnames output
test_that("FindVars() returns column names of df-like 
          structure based on partial overlap with character argument",
          #test with known output length 4 from example
          {test <- FindVars(varnames = c("^c", "g"),
                            allnames = colnames(mtcars))
          # test$names: "mpg"  "cyl"  "gear" "carb"
          expect_equal(test$count, 4)}
)

#test the beginning-of-word punctuation (`^(letter)` function) arg works
test_that("FindVars() works with place sensitivity specifications from the user",
          #removing the first-letter argument should return one more column name
          {test1 <- FindVars(varnames = c("^c", "g"),
                             allnames = colnames(mtcars))
          test2 <- FindVars(varnames = c("c", "g"),
                            allnames = colnames(mtcars))
          #test2$names: "mpg"  "cyl"  "qsec"  "gear" "carb"
          expect_equal((test2$count - test1$count), 1)}
)


#test the exclusion parameter
test_that("FindVars() 'exclude=' works",
          {#test1 call should remove "gear" and "carb" compared 
            #to test2$names
          test1 <- FindVars(varnames = c("^c", "g"),
                            allnames = colnames(mtcars),
                            exclude = "r")
          test2 <- FindVars(varnames = c("^c", "g"),
                            allnames = colnames(mtcars))
          expect_equal((test2$count - test1$count), 2)}
)

#test the index returns? not sure whether worth it
