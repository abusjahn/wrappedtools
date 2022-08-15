#' Predefined sets of labels for plots with log-scaled axes
#'
#' \code{logrange_1} returns a vector for log-labels at .1, 1, 100, 1000 ...
#'
#' @return numeric vector
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   scale_y_log10(breaks = logrange_5)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   scale_y_log10(breaks = logrange_123456789)
#' @export
logrange_1 <- c(1) * rep(10^(-20:20), each = 1)

#' @describeIn logrange_1 vector for log-labels at
#' 1.0, 1.5, 2.0, 2.5 ... 10, 15, 20, 25 ...
#'
#' @return numeric vector
#'
#' @export
logrange_5 <- seq(1, 9.5, .5) * rep(10^(-20:20), each = 18)

#' @describeIn logrange_1 vector for log-labels at
#' 1, 2, 3 ... 9, 10, 20, 30 ... 90, 100 ...
#'
#' @export
logrange_123456789 <- c(1:9) * rep(10^(-20:20), each = 9)

#' @describeIn logrange_1 vector for log-labels at
#' 1 ,2, 3, 5, 7, 10, 20 ,30, 50, 70 ...

#' @export
logrange_12357 <- c(1, 2, 3, 5, 7) * rep(10^(-20:20), each = 5)

#' @describeIn logrange_1 vector for log-labels at
#' 1, 5, 10, 50 ...

#' @export
logrange_15 <- c(1, 5) * rep(10^(-20:20), each = 2)
