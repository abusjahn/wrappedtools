.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Package wrappedtools is still experimental,",
                              'be warned that there might be dragons'))
}
.onLoad <- function(libname, pkgname) {
  options(stringsAsFactors=F)
}
#'@import tidyverse
NULL


#'@importFrom stats anova as.formula confint cor.test fisher.test ks.test mad median na.omit p.adjust pairwise.t.test pnorm power.prop.test power.t.test qnorm quantile rpois sd wilcox.test 
NULL

#'@importFrom magrittr %<>% 
NULL

