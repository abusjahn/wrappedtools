.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Package wrappedtools is still experimental,",
                              'be warned that there might be dragons'))
}
.onLoad <- function(libname, pkgname) {
  #options(stringsAsFactors=F)
}
#'@import tibble
NULL

#'@import dplyr
NULL

#'@import stringr
NULL


#'@importFrom stats anova as.formula confint cor.test fisher.test ks.test mad median na.omit p.adjust pairwise.t.test pnorm power.prop.test power.t.test qnorm quantile rpois sd wilcox.test t.test var.test
NULL

#'@importFrom magrittr %<>% %>%
#'@name%<>%
#'export
NULL

#'@import ggplot2 
NULL

#'@import rlang
NULL

#'@importFrom tidyr gather spread separate nest
NULL

#'@importFrom utils data
NULL

#'@import tidyverse
NULL


