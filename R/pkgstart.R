.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("Package wrappedtools is still experimental,",
                              'be warned that there might be dragons'))
}
.onLoad <- function(libname, pkgname) {
  options(stringsAsFactors=F)
}
