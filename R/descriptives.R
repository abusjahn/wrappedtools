#' Compute mean and sd and put together with the ± symbol.
#'
#' @param x Data for computation.
#' @param roundDig Number of relevant digits for roundR.
#' @param drop0 Should trailing zeros be dropped?
#' @param groupvar Optional grouping variable for subgroups.
#' @param range Should min and max be included in output?
#' @param rangesep How should min/max be separated from mean+-sd?
#' @param add_n Should n be included in output?
#' @param add_n Should n be included in output?
#' @param .german logical, should "." and "," be used as bigmark and decimal?
#' @return character vector with mean ± SD, rounded to desired precision
#'
#' @examples
#' # basic usage of meansd
#' meansd(x = mtcars$wt)
#' # with additional options
#' meansd(x = mtcars$wt, groupvar = mtcars$am, add_n = TRUE)
#' @export
meansd <- function(x, roundDig = 2, drop0 = FALSE, groupvar = NULL,
                   range = FALSE, rangesep = " ", add_n = FALSE, .german = FALSE) {
  out <- ""
  if (length(na.omit(x)) > 0) {
    if (is.null(groupvar)) {
      meansd <- cbind(
        matrix(c(
          mean(x, na.rm = TRUE),
          sd(x, na.rm = TRUE),
          min(x, na.rm = TRUE),
          max(x, na.rm = TRUE)
        ),
        ncol = 4, byrow = FALSE
        ),
        length(na.omit(x))
      )
      meansd[1:2] %<>%
        roundR(level = roundDig, drop0 = drop0, .german = .german)
      meansd[3:4] %<>%
        roundR(level = roundDig, drop0 = drop0, .german = .german)
    } else {
      meansd <- matrix(c(
        by(x, groupvar, mean, na.rm = TRUE),
        by(x, groupvar, sd, na.rm = TRUE),
        by(x, groupvar, min, na.rm = TRUE),
        by(x, groupvar, max, na.rm = TRUE)
      ),
      ncol = 4, byrow = FALSE
      ) %>%
        na_if(Inf) %>%
        na_if(-Inf)
      meansd[, 1:2] %<>%
        roundR(level = roundDig, drop0 = drop0, .german = .german)
      meansd[, 3:4] %<>%
        # as.numeric() %>%
        roundR(level = roundDig, drop0 = drop0, .german = .german)
      meansd %<>%
        cbind(by(x, groupvar, function(x) {
          length(na.omit(x))
        }))
    }
    out <- paste(meansd[, 1], meansd[, 2], sep = " \u00B1 ")
    if (range) {
      out <- paste0(
        out, rangesep, " [",
        apply(matrix(meansd[, 3:4], ncol = 2), 1, paste,
          collapse = " -> "
        ), "]"
      ) # \u22ef
    }
    if (add_n) {
      out <- paste0(
        out, rangesep, " [n=",
        meansd[, 5], "]"
      ) # \u22ef
    }
  } #   }
  return(out)
}

#' Compute median and quartiles and put together.
#'
#' @param x Data for computation.
#' @param nround Number of digits for fixed round.
#' @param probs Quantiles to compute.
#' @param qtype Type of quantiles.
#' @param roundDig Number of relevant digits for roundR.
#' @param drop0 Should trailing zeros be dropped?
#' @param groupvar Optional grouping variable for subgroups.
#' @param range Should min and max be included in output?
#' @param rangesep How should min/max be separated from mean+-sd?
#' @param rangearrow What is put between min -> max?
#' @param prettynum logical, apply prettyNum to results?
#' @param .german logical, should "." and "," be used as bigmark and decimal?
#' @param add_n Should n be included in output?
#' @return character vector with median \code{[1stQuartile/3rdQuartile]}, rounded to desired precision
#' @examples
#' # basic usage of median_quart
#' median_quart(x = mtcars$wt)
#' # with additional options
#' median_quart(x = mtcars$wt, groupvar = mtcars$am, add_n = TRUE)
#' data(faketrial)
#' median_quart(x=faketrial$`Biomarker 1 [units]`,groupvar = faketrial$Treatment)
#' @export
median_quart <- function(x, nround = NULL, probs = c(.25, .5, .75),
                         qtype = 8, roundDig = 2, drop0 = FALSE,
                         groupvar = NULL, range = FALSE, rangesep = " ",
                         rangearrow = " -> ",
                         prettynum = FALSE, .german = FALSE, add_n = FALSE) {
  out <- " "
  bigmark <- ifelse(.german, ".", ",")
  decimal <- ifelse(.german, ",", ".")
  if (length(na.omit(x)) >= 1) {
    if (is.null(groupvar)) {
      quart <- matrix(
        c(
          stats::quantile(x, probs = c(probs, 0, 1), na.rm = TRUE, type = qtype),
          length(na.omit(x))
        ),
        ncol = length(probs) + 3
      )
    } else {
      quart <- matrix(
        unlist(
          by(x, groupvar, quantile,
            probs = c(probs, 0, 1), na.rm = TRUE,
            type = qtype
          )
        ),
        ncol = length(probs) + 2, byrow = TRUE
      )
      quart <- cbind(
        quart,
        unlist(by(
          x, groupvar, function(x) {
            length(na.omit(x))
          }
        ))
      )
    }
    if (is.null(nround)) {
      colcount <- ncol(quart)
      quart[, 1:(colcount - 3)] <- roundR(quart[, 1:(colcount - 3)],
        level = roundDig, drop0 = drop0, .german = .german
      )
      quart[, (colcount - 2):(colcount - 1)] <-
        roundR(as.numeric(quart[, (colcount - 2):(colcount - 1)]),
          level = roundDig, drop0 = drop0, .german = .german
        )
      if (prettynum) {
        #   quart <- apply(quart,1:2,function(x){
        #     formatC(as.numeric(x),
        #             digits = roundDig-1,
        #             format = 'f',
        #             big.mark = bigmark,
        #             decimal.mark = decimal,
        #             preserve.width = 'common',drop0trailing = FALSE)})
      }
    } else {
      quart[, -ncol(quart)] <- round(quart[, -ncol(quart)], nround)
      if (prettynum) {
        quart <- apply(quart, 1:2, function(x) {
          formatC(as.numeric(x),
            digits = nround,
            format = "f",
            big.mark = bigmark,
            decimal.mark = decimal,
            preserve.width = "common", drop0trailing = FALSE
          )
        })
      }
    }
    out <- str_glue("{quart[,2]} ({quart[,1]}/{quart[,3]})")
    if (range) {
      out <- str_glue("{out}{rangesep} [\\
                      {apply(matrix(quart[,(length(probs)+1):(length(probs)+2)],ncol=2),1,glue::glue_collapse,
                      sep=rangearrow)}]")
    }
    if (add_n) {
      out <- str_glue("{out}{rangesep} [n={quart[,length(probs)+3]}]")
    }
  }
  out <- as.character(out)
  return(out)
}


#' Compute mean and standard error of mean and put together with the ± symbol.
#'
#' \code{meanse} computes SEM based on Standard Deviation/square root(n)
#' @param x Data for computation.
#' @param roundDig Number of relevant digits for roundR.
#' @param drop0 Should trailing zeros be dropped?
#' @param mult multiplier for SEM, default 1, can be set to
#' e.g. 2 or 1.96 to create confidence intervals
#'
#' @return character vector with mean ± SEM, rounded to desired precision
#'
#' @examples
#' # basic usage of meanse
#' meanse(x = mtcars$wt)
#' @export
meanse <- function(x, mult = 1, roundDig = 2, drop0 = FALSE) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  ms <- roundR(c(m, s * mult),
    level = roundDig, drop0 = drop0
  )
  out <- paste(ms[1], ms[2], sep = "\u00B1")
  return(out)
}


#' Compute standard error of median.
#'
#' \code{median_cl_boot} is based on \code{\link{mad}}/square root(n)
#'
#' @param x Data for computation.
#'
#' @return numeric vector with SE Median.
#'
#' @examples
#' # basic usage of meanse
#' se_median(x = mtcars$wt)
#' @export
se_median <- function(x) {
  mad(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

#' Compute confidence interval of median by bootstrapping.
#'
#' \code{median_cl_boot} computes lower and upper confidence limits for the
#' estimated median, based on bootstrapping.
#'
#' @param x Data for computation.
#' @param conf confidence interval with default 95%.
#' @param type type for function boot.ci.
#' @param nrepl number of bootstrap replications, defaults to 1000.
#'
#' @return A tibble with one row and three columns: Median, CIlow, CIhigh.
#'
#' @examples
#' # basic usage of meanse
#' median_cl_boot(x = mtcars$wt)
#' @export
median_cl_boot <- function(x, conf = 0.95, type = "basic", nrepl = 10^3) {
  x <- na.omit(x)
  lconf <- (1 - conf) / 2
  uconf <- 1 - lconf
  bmedian <- function(x, ind) median(x[ind], na.rm = TRUE)
  bt <- boot::boot(x, bmedian, nrepl)
  bb <- boot::boot.ci(bt, type = type)
  tibble(
    Median = median(x, na.rm = TRUE),
    CIlow = quantile(bt$t, lconf),
    CIhigh = quantile(bt$t, uconf)
  )
}

#' Compute absolute and relative frequencies.
#'
#' \code{cat_desc_stats} computes absolute and relative frequencies for
#' categorical data with a number of formatting options.
#'
#' @param quelle Data for computation.
#' @param separator delimiter between results per level, preset as ' '.
#' @param return_level Should levels be reported?
#' @param ndigit Digits for rounding of relative frequencies.
#' @param groupvar Optional grouping factor.
#' @param singleline Put all group levels in  a single line?
#' @param percent Logical, add percent-symbol after relative frequencies?
#' @param prettynum logical, apply prettyNum to results?
#' @param .german logical, should "." and "," be used as bigmark and decimal?
#' Sets prettynum to TRUE.
#'
#' @return
#' Structure depends on parameter return_level:
#' if FALSE than a tibble with descriptives, otherwise a list with two tibbles
#' with levels of factor and descriptives.
#' If parameter singleline is FALSE (default), results for each factor level is
#' reported in a separate line, otherwise they are pasted.
#' Number of columns for result tibbles is one or number of levels of the
#' additional grouping variable.
#'
#' @examples
#' cat_desc_stats(mtcars$gear)
#' cat_desc_stats(mtcars$gear, return_level = FALSE)
#' cat_desc_stats(mtcars$gear, groupvar = mtcars$am)
#' cat_desc_stats(mtcars$gear, groupvar = mtcars$am, singleline = TRUE)
#' @export
cat_desc_stats <- function(quelle, separator = " ",
                           return_level = TRUE,
                           ndigit = 0,
                           groupvar = NULL,
                           singleline = FALSE,
                           percent = TRUE,
                           prettynum = FALSE,
                           .german = FALSE) {
  percent <- ifelse(percent, "%", "")
  bigmark <- ifelse(.german, ".", ",")
  decimal <- ifelse(.german, ",", ".")
  if (!is.factor(quelle)) {
    # if(is.numeric(quelle)) {
    #   quelle<-factor(quelle,
    #                  levels=sort(unique(quelle)),
    #                  labels=sort(unique(quelle)))
    # } else {
    quelle <- factor(quelle)
  }
  level <- levels(quelle) %>% enframe(name = NULL)
  if (singleline) {
    level <- paste(levels(quelle), sep = "", collapse = separator)
  }
  if (is.null(groupvar)) {
    tableout <- matrix(table(quelle),
      nrow = length(levels(quelle)),
      byrow = FALSE
    )
    colnames(tableout) <- "abs"
    pt_temp <- round(
      100 * prop.table(tableout),
      ndigit
    )
    if (.german) {
      prettynum <- TRUE
    }
    if (prettynum) {
      pt_temp <- formatC(pt_temp,
        digits = ndigit,
        format = "f",
        big.mark = bigmark,
        decimal.mark = decimal,
        preserve.width = "common", drop0trailing = FALSE
      )
      tableout <- formatC(tableout,
        digits = 0,
        format = "f",
        big.mark = bigmark,
        decimal.mark = decimal,
        preserve.width = "common"
      )
    }
    ptableout <- matrix(paste0(
      " (",
      pt_temp,
      percent, ")"
    ),
    nrow = length(levels(quelle)),
    byrow = FALSE
    )
    colnames(ptableout) <- "rel"
  } else {
    tableout <- matrix(unlist(by(quelle, groupvar, table)),
      nrow = length(levels(quelle)),
      byrow = FALSE
    )
    colnames(tableout) <- glue::glue("abs{levels(factor(groupvar))}")

    pt_temp <- round(100 * prop.table(tableout, margin = 2), ndigit)
    if (prettynum) {
      pt_temp <- formatC(pt_temp,
        digits = ndigit,
        format = "f",
        big.mark = bigmark,
        decimal.mark = decimal,
        preserve.width = "common", drop0trailing = FALSE
      )
      tableout <- formatC(tableout,
        digits = 0,
        format = "f",
        big.mark = bigmark,
        decimal.mark = decimal,
        preserve.width = "common"
      )
    }
    ptableout <- matrix(
      paste0(
        " (", pt_temp,
        percent, ")"
      ),
      nrow = length(levels(quelle)),
      byrow = FALSE
    )
    colnames(ptableout) <- glue::glue("rel{levels(factor(groupvar))}")
  }
  zwert <- purrr::map2(tableout, ptableout, glue::glue) %>%
    as.character() %>%
    matrix(
      nrow = length(levels(quelle)),
      byrow = FALSE
    ) %>%
    as_tibble(.name_repair = "minimal")
  if (is.null(groupvar)) {
    colnames(zwert) <- "desc"
  } else {
    colnames(zwert) <- glue::glue("desc{levels(factor(groupvar))}")
  }
  if (singleline) {
    zwert <- purrr::map(zwert,
      .f = function(x) {
        glue::glue_collapse(x, sep = separator)
      }
    ) %>%
      as_tibble()
  }
  levdesstats <- list(level = level, freq = zwert)
  if (return_level == TRUE) {
    return(levdesstats)
  } else {
    return(zwert)
  }
}

#' Compute coefficient of variance.
#'
#' \code{var_coeff computes relative variability as standard deviation/mean *100}
#'
#' @param x Data for computation.
#'
#' @return numeric vector with coefficient of variance.
#'
#' @examples
#' var_coeff(x = mtcars$wt)
#' @export
var_coeff <- function(x) {
  return(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100)
}

#' Standard Error of Mean.
#'
#' \code{SEM} computes standard error of mean.
#'
#' @param x Data for computation.
#'
#' @return numeric vector with SEM.
#'
#' @examples
#' SEM(x = mtcars$wt)
#' @export
SEM <- function(x) {
  return(sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))))
}
