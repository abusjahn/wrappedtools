# Author: Bilal
# First time testing a more complex function in a programming package of any kind


# First we want to lay out the possible testable properties of this function output
# -list lengths match each other in testXcnn 
# -with a table element known through manual testing or ideally reliably auto-calculated
# -

# Errors that were thrown as I manually tested my work on the script:
#   -joined list dimensions in tibble
#   -rawdata items being named improperly



test_that("compare_n_numvars() creates the same lengths of lists when creating the raw table", {
  # This gets you a random test dataset out of the list in the line below
  # Random dataset out of 3: all dataset[[1 to 4]] columns are numerical dep_vars, 
  # dataset[[5]] is an ordinal indep_var
  # 
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
  
  tibble_itself <- RandomTestDatasetOrd[[1]]
  indep_var <- as.character(names(tibble_itself)[[5]])
  dep_vars <- c(names(tibble_itself)[[1]],
                names(tibble_itself)[[2]],
                names(tibble_itself)[[3]],
                names(tibble_itself)[[4]])
  
  ord_test_in <- compare_n_numvars(.data = tibble_itself,
                               dep_vars = dep_vars,
                               indep_var = indep_var,
                               gaussian = F)
  
  expect_equal(length(ord_test_in$raw$Variable), length(ord_test_in$raw$data))
  expect_equal(length(ord_test_in$raw$data), length(ord_test_in$raw$desc_tab))
  expect_equal(length(ord_test_in$raw$desc_tab), length(ord_test_in$raw$desc_grp))
  expect_equal(length(ord_test_in$raw$desc_grp), length(ord_test_in$raw$anova_out))
  expect_equal(length(ord_test_in$raw$anova_out), length(ord_test_in$raw$`p_wcox/t_out`))
  expect_equal(length(ord_test_in$raw$desc_grp), length(ord_test_in$raw$p_wcox_t_out))
  
  # I will now test Gaussian variables by shifting the column reads down one index
  
  RandomTestDatasetGau <- rlist::list.sample(.data=
                                               list(
                                                 tibble(mtcars$mpg,
                                                        mtcars$wt,
                                                        mtcars$hp,
                                                        mtcars$disp,
                                                        .name_repair = 'unique'),
                                                 tibble(iris),
                                                 tibble(ggplot2::diamonds$x, 
                                                        ggplot2::diamonds$y, 
                                                        ggplot2::diamonds$z, 
                                                        ggplot2::diamonds$cut,
                                                        .name_repair = 'unique')),
                                             size=1)
  
  tibble_itself <- RandomTestDatasetGau[[1]]
  indep_var <- as.character(names(tibble_itself)[[4]])
  dep_vars <- c(names(tibble_itself)[[1]],
                names(tibble_itself)[[2]],
                names(tibble_itself)[[3]])
  
  gau_test_in <- compare_n_numvars(.data = tibble_itself,
                                   dep_vars = dep_vars,
                                   indep_var = indep_var,
                                   gaussian = T)
  
  expect_equal(length(gau_test_in$raw$Variable), length(gau_test_in$raw$data))
  expect_equal(length(gau_test_in$raw$data), length(gau_test_in$raw$desc_tab))
  expect_equal(length(gau_test_in$raw$desc_tab), length(gau_test_in$raw$desc_grp))
  expect_equal(length(gau_test_in$raw$desc_grp), length(gau_test_in$raw$anova_out))
  expect_equal(length(gau_test_in$raw$anova_out), length(gau_test_in$raw$`p_wcox/t_out`))
  expect_equal(length(gau_test_in$raw$desc_grp), length(gau_test_in$raw$p_wcox_t_out))
  
})