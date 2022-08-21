# test whether length(names(groupvar)) is the same as length(output)
test_that("meansd(groupvar=) splits the x argument 
          into the amount of groups that it should",
          {#Create random dataset as in test-cnnumvars.R
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
            
            tibble_itself <- RandomTestDatasetGau[[1]]
            col <- as.numeric(sample(length(tibble_itself),1))
            test <- meansd(tibble_itself[[col]])
            #mean
            exp_out_mean <- mean(tibble_itself[[col]])
            #if -1>mean<1
            if (-1<exp_out_mean&exp_out_mean<1) {
              exp_out_mean <- signif(exp_out_mean, digits = 2) # rounds to 2 sig digits
              #makes sure you get one decimal place like the function
              if (nchar(exp_out_mean)!=3) {
                exp_out_mean = round(exp_out_mean, digits = 1)
              }} else if (-10<exp_out_mean&exp_out_mean<10) {
                exp_out_mean <-  round(exp_out_mean, digits = 1)
              } # otherwise
            else{ # if -10>mean|mean>10, cut off decimal places
              exp_out_mean = round(exp_out_mean,digits = 0)
            }
            #sd
            exp_out_sd <- sd(tibble_itself[[col]])
            #if -1>sd<1
            if (-1<exp_out_sd&exp_out_sd<1) {
              exp_out_sd <- signif(exp_out_sd, digits = 2) # rounds to 2 sig digits
              #makes sure you get one decimal place like the function
              if (nchar(exp_out_sd)!=3) {
                exp_out_sd = round(exp_out_sd, digits = 1)
              }} else if (-10<exp_out_sd&exp_out_sd<10) {
                exp_out_sd <-  round(exp_out_sd, digits = 1)
              } # otherwise
            else{ # if -10>sd|sd>10, cut off decimal places
              exp_out_sd = round(exp_out_sd,digits = 0)
            }
            exp_out <- paste0(exp_out_mean," ± ",exp_out_sd)
            expect_equal(test, as.character(exp_out))
          }
)