# Call 
#     for (i in 1:10) {devtools::test()}
#
test_that("meansd() does what it says on random columns of numerical data\n(error may be thrown if e.g. an sd of 1.0 is selected)",
          {#Create random dataset in list of 1 as in gaussian test-cnnumvars.R
            #because you don't want categorical or ordinal variables in here
            RandomTestDatasetGau <- rlist::list.sample(.data=
                                                         list(
                                                           tibble(mtcars$mpg,
                                                                  mtcars$wt,
                                                                  mtcars$hp,
                                                                  mtcars$disp,
                                                                  .name_repair = 'unique'),
                                                           tibble(iris$Sepal.Length,
                                                                  iris$Sepal.Width,
                                                                  iris$Petal.Length,
                                                                  iris$Petal.Width),
                                                           tibble(ggplot2::diamonds$x, 
                                                                  ggplot2::diamonds$y, 
                                                                  ggplot2::diamonds$z, 
                                                                  ggplot2::diamonds$carat,
                                                                  .name_repair = 'unique')),
                                                       size=1)
            #Unlist the dataset
            tibble_itself <- RandomTestDatasetGau[[1]]
            #Random column seed
            col <- as.numeric(sample(length(tibble_itself),1))
            #Test call in named object
            test <- meansd(tibble_itself[[col]])
            #mean of random column
            exp_out_mean <- mean(tibble_itself[[col]])
            #if -1>mean<1
            # rounds to 2 sig digits
            if (-1<exp_out_mean&exp_out_mean<1) {
              exp_out_mean <- signif(exp_out_mean, digits = 2)
              #makes sure you get one decimal place like the function
              if (nchar(exp_out_mean)!=3) {
                exp_out_mean = round(exp_out_mean, digits = 2)
              }
            } else if (-10<exp_out_mean&exp_out_mean<10) {
              exp_out_mean <-  round(exp_out_mean, digits = 1)
            } else {
              # otherwise if -10>mean|mean>10, cut off decimal places
              exp_out_mean = round(exp_out_mean,digits = 0)
            }
            #sd
            exp_out_sd <- sd(tibble_itself[[col]])
            #if -1>sd<1 rounds to 2 sig digits
            if (-1<exp_out_sd&exp_out_sd<=1) {
              exp_out_sd <- signif(exp_out_sd, digits = 2) 
              #makes sure you get one decimal place like the function
              if (nchar(exp_out_sd)!=3) {
                exp_out_sd = round(exp_out_sd, digits = 1)
              }
            } else if (-10<exp_out_sd&exp_out_sd<10) {
              exp_out_sd <-  round(exp_out_sd, digits = 1)
            } else {
              # otherwise if -10>sd|sd>10, cut off decimal places
              exp_out_sd = round(exp_out_sd,digits = 0)
            }
            exp_out <- paste0(exp_out_mean," ± ",exp_out_sd)
            expect_equal(test, as.character(exp_out))
          }
)
# test whether length(names(groupvar)) is the same as length(output)
test_that("meansd(groupvar=) splits the x argument 
into the amount of groups that it should",{
  RandomTestDatasetOrd <- rlist::list.sample(.data=
                                               list(
                                                 tibble(mtcars$mpg,
                                                        mtcars$wt,
                                                        mtcars$hp,
                                                        mtcars$drat,
                                                        mtcars$cyl,
                                                        .name_repair = 'unique'),
                                                 tibble(iris),
                                                 tibble(ggplot2::diamonds$carat,
                                                        ggplot2::diamonds$x, 
                                                        ggplot2::diamonds$y, 
                                                        ggplot2::diamonds$z, 
                                                        ggplot2::diamonds$cut,
                                                        .name_repair = 'unique')),
                                             size=1)
  #Unlist the dataset
  tibble_itself <- RandomTestDatasetOrd[[1]]
  #Random column seed
  col <- as.numeric(sample((length(tibble_itself)-1),1))
  #compare against the ordinal group
  #Test call in named object
  test <- meansd(tibble_itself[[col]],groupvar = tibble_itself[[5]]) # column 5 is the ordinal grouping variable we supplied
  expect_length(test, length(unique(tibble_itself[[5]])))
}
)