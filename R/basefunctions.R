utils::globalVariables(c("rawdata", "stratum"))
#' Automatic rounding to a reasonable length, based on largest number
#'
#' \code{roundR} takes a vector or matrix of numbers and returns rounded values
#' with selected precision and various formatting options.
#'
#' @param roundin A vector or matrix of numbers.
#' @param smooth A logical specifying if you want rounding before the dot
#' (e.g. 12345 to 12300).
#' @param level A number specifying number of relevant digits to keep.
#' @param textout A logical if output is converted to text.
#' @param drop0 A logical if trailing zeros should be dropped.
#' @param .german A logical if german numbers should be reported.
#' @param .bigmark A logical if big.mark is to be shown, mark itself
#' depends on parameter .german.
#'
#' @return vector of type character (default) or numeric,
#' depending on parameter textout.
#'
#' @examples
#' roundR(1.23456, level = 3)
#' roundR(1.23456, level = 3, .german = TRUE)
#' roundR(1234.56, level = 2, smooth = TRUE)
#' @export
roundR <- function(roundin, level = 2, smooth = FALSE,
                   textout = TRUE, drop0 = FALSE, .german = FALSE, .bigmark = FALSE) {
  if (.german) {
    textout <- TRUE
  }
  decimalmark <- ifelse(.german, ",", ".")
  bigmark <- ifelse(.german, ".", ",")
  if (!.bigmark) {
    bigmark <- ""
  }
  if (!is.matrix(roundin)) {
    roundin <- matrix(roundin)
  }
  roundin <- as.numeric(roundin)
  roundout <- roundin
  roundlevel <- 0
  roundlevel <- max(
    0,
    level - floor(
      log10(
        max(abs(roundin), na.rm = TRUE)
      ) + 1
    )
  )
  roundout[which(!is.na(roundout))] <-
    round(roundin[which(!is.na(roundin))], roundlevel)
  if (smooth & max(abs(roundout), na.rm = TRUE) != 0) {
    roundout[which(!is.na(roundout))] <-
      round(
        roundin[which(!is.na(roundin))] /
          10^ceiling(log10(max(abs(roundin), na.rm = TRUE)) - level)
      ) *
        10^ceiling(log10(max(abs(roundin), na.rm = TRUE)) - level)
  }
  if (textout) {
    roundout[which(!is.na(roundout))] <-
      formatC(roundout[which(!is.na(roundout))],
        format = "f",
        digits = roundlevel, drop0trailing = drop0,
        big.mark = bigmark,
        decimal.mark = decimalmark
      )
  }
  return(roundout)
}

#' Convert significance levels to symbols
#'
#' \code{markSign} returns the symbol associated with a significance level.
#'
#' @param SignIn A single p-value.
#' @param plabel A translation table, predefined with the usual symbols.
#'
#' @returns factor with label as defined in plabel.
#'
#' @examples
#' markSign(0.012)
#' @export
markSign <- function(SignIn, plabel = c("n.s.", "+", "*", "**", "***")) {
  SignIn <- as.numeric(SignIn)
  SignOut <- cut(SignIn,
    breaks = c(-Inf, .001, .01, .05, .1, 1),
    labels = rev(plabel)
  )
  return(SignOut)
}

#' Re-format p-values, avoiding rounding to 0
#'
#' \code{formatP} simplifies p-values by rounding to the maximum of p or a
#' predefined level. Optionally < or = can be added, as well as
#' symbols according to significance level.
#'
#' @param pIn A numeric vector or matrix with p-values.
#' @param ndigits Number of digits (default=3).
#' @param textout Cast output to character (default=TRUE)?
#' @param pretext Should = or < be added before p (default=FALSE)?
#' @param mark Should significance level be added after p (default=FALSE)?
#' @param german_num change dot (default) to comma?
#'
#' @returns vector or matrix (depending on type of pIn) with type character (default) or numeric,
#' depending on parameter textout
#'
#' @examples
#' formatP(0.012345)
#' formatP(0.012345, ndigits = 4)
#' formatP(0.000122345, ndigits = 3, pretext = TRUE)
#' @export
formatP <- function(pIn, ndigits = 3, textout = TRUE, pretext = FALSE, mark = FALSE,
                    german_num = FALSE) {
  decimal.mark <- ifelse(german_num, ",", ".")
  formatp <- ""
  if (is.numeric(pIn)) {
    pIn_is_matrix <- is.matrix(pIn)
    if (!pIn_is_matrix) {
      pIn <- matrix(pIn)
    }
    formatp <- apply(
      X = pIn, MARGIN = c(1, 2), max,
      10**(-ndigits), na.rm = FALSE
    ) %>%
      apply(MARGIN = c(1, 2), round, ndigits) %>%
      apply(
        MARGIN = c(1, 2),
        formatC, format = "f",
        digits = ndigits, drop0trailing = FALSE,
        decimal.mark = decimal.mark
      )
    if (pretext) {
      for (row_i in 1:nrow(pIn)) {
        for (col_i in 1:ncol(pIn)) {
          formatp[row_i, col_i] <- paste(
            ifelse(pIn[row_i, col_i] < 10**(-ndigits),
              "<", "="
            ),
            formatp[row_i, col_i]
          )
        }
      }
    }
    if (mark) {
      formatp <- matrix(
        paste(
          formatp,
          apply(gsub("[\\<\\=]", "", formatp), c(1, 2), markSign)
        ),
        ncol = ncol(pIn)
      )
    }
    if (textout == FALSE & pretext == FALSE) {
      formatp <- apply(formatp, MARGIN = c(1, 2), as.numeric)
    }
  }
  if(!pIn_is_matrix){
    formatp <- as.vector(formatp)
  }
  return(formatp)
}


#' Find numeric index and names of columns based on patterns
#'
#' \code{FindVars} looks up colnames (by default for data-frame rawdata)
#' based on parts of names, using regular expressions.
#' Exlusion rules may be specified as well.
#'
#' @param varnames Vector of pattern to look for.
#' @param allnames Vector of values to detect pattern in;
#' by default, colnames(rawdata).
#' @param exact Partial matching or exact only (adding ^ and $)?
#' @param exclude Vector of pattern to exclude from found names.
#' @param casesensitive Logical if case is respected in matching (default FALSE: a<>A)
#' @param fixed Logical, treat pattern as regex?
#' @export
#' @return A list with index, names, backticked names, and symbols
#' @examples
#' FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars))
#' FindVars(varnames = c("^c", "g"), allnames = colnames(mtcars), exclude = "r")
#' rawdata <- mtcars
#' FindVars(varnames = c("^c", "g"))
FindVars <- function(varnames, allnames = NULL,
                     exact = FALSE, exclude = NA, casesensitive = TRUE,
                     fixed = FALSE) {
  if (is.null(allnames)) {
    allnames <- colnames(get("rawdata"))
  }
  if (fixed) {
    exact <- FALSE
  }
  allnames_tmp <- allnames
  if (!casesensitive) {
    varnames <- tolower(varnames)
    allnames_tmp <- tolower(allnames)
    exclude <- tolower(exclude)
  }
  vars <- numeric()
  evars <- numeric()
  if (exact) {
    for (i in 1:length(varnames)) {
      vars <- c(vars, grep(paste0("^", varnames[i], "$"), allnames_tmp))
    }
    vars <- unique(vars)
  } else {
    for (i in 1:length(varnames)) {
      vars <- c(vars, grep(varnames[i], allnames_tmp,
        fixed = fixed
      ))
    }
    vars <- sort(unique(vars))
    if (any(!is.na(exclude))) {
      for (i in 1:length(exclude))
      {
        evars <- c(evars, grep(exclude[i], allnames_tmp))
      }
      evars <- unique(na.omit(match(
        sort(unique(evars)), vars
      )))
      if (length(evars) > 0) {
        vars <- vars[-evars]
      }
    }
    vars <- unique(vars)
  }
  return(list(
    index = vars,
    names = allnames[vars],
    bticked = bt(allnames[vars]),
    symbols = rlang::syms(allnames[vars]),
    count = length(vars)
  ))
}

#' Enhanced kable with definable number of rows/columns for splitting
#'
#' \code{print_kable} formats and prints tibbles/df's in markdown with splitting
#' into sub-tables with repeated caption and header.
#'
#' @param t table to print.
#' @param nrows number of rows (30) before splitting.
#' @param ncols number of columns (100) before splitting.
#' @param caption header.
#' @param ... Further arguments passed to [kable].
#' @return No return value, called for side effects.
#' @export
print_kable <- function(t, nrows = 30, caption = "",
                        ncols = 100, ...) {
  # require(knitr)
  for (block_i in 1:ceiling(nrow(t) / nrows)) {
    for (col_i in 1:ceiling((ncol(t) - 1) / ncols)) {
      if (block_i + col_i > 2) {
        cat("\\newpage\n\n")
      }
      print(
        knitr::kable(
          t[
            (1 + (block_i - 1) * nrows):
            min(nrow(t), block_i * nrows),
            c(1, (2 + (col_i - 1) * ncols):min((1 + col_i * ncols), ncol(t)))
          ],
          row.names = FALSE,
          caption = paste0(
            ifelse(block_i + col_i > 2, "continued: ", ""),
            caption,
            "  \n  \n   "
          )
        )
      )
      cat("  \n   \n")
    }
  }
}

#' Enhanced kable with latex
#'
#' \code{pdf_kable} formats tibbles/df's for markdown
#'
#' @param .input table to print
#' @param twidth Default 14
#' @param width1 Width of 1st column, default 6.
#' @param tposition Default left
#' @param innercaption subheader
#' @param caption header
#' @param foot footnote
#' @param escape see kable
#'
#'@return A character vector of the table source code. 
#' @export
pdf_kable <- function(.input, width1 = 6,
                      twidth = 14,
                      tposition = "left",
                      innercaption = NULL,
                      caption = "",
                      foot = NULL,
                      escape = TRUE) {
  ncols <- ncol(.input)
  out <- knitr::kable(.input,
    format = "latex", booktabs = TRUE,
    linesep = "",
    escape = escape, caption = caption,
    align = c("l", rep("c", ncols - 1))
  ) %>%
    kableExtra::kable_styling(
      position = tposition,
      latex_options = c(
        "striped",
        "hold_position"
      )
    ) %>%
    kableExtra::column_spec(-1, # border_left = TRUE,
      width = paste0((twidth - width1) / (ncols - 1), "cm"),
    ) %>%
    kableExtra::column_spec(1, bold = TRUE, width = paste0(width1, "cm")) %>%
    kableExtra::row_spec(0, bold = TRUE)
  if (!is.null(innercaption)) {
    caption1 <- c(caption = ncols)
    names(caption1) <- caption
    out <- out %>%
      kableExtra::add_header_above(caption1, bold = TRUE)
  }
  if (!is.null(foot)) {
    out <- out %>%
      kableExtra::footnote(general = foot)
  }
  return(out)
}


#' Shortcut for colnames()
#'
#' \code{cn} lists column names, by default for variable rawdata.
#'
#' @param data Data structure to read column names from.
#' 
#' @return Character vector with column names.
#' 
#' @examples 
#' cn(mtcars)
#' @export
cn <- function(data = rawdata) {
  colnames(data)
}

#' Add backticks to names or remove them
#'
#' \code{bt} adds leading and trailing backticks to make illegal variable names
#' usable. Optionally removes them.
#'
#' @param x Names to add backtick to.
#' @param remove Option to remove existing backticks, default=FALSE.
#' 
#' @return Character vector with backticks added.
#' 
#' @examples
#' bt('name 1')
#' 
#' @export
bt <- function(x, remove = FALSE) {
  if (remove) {
    return(gsub("`", "", x))
  } else {
    return(paste0("`", x, "`"))
  }
}


#' Search within data.frame or tibble
#'
#' \code{tab.search} searches for pattern within a data-frame or tibble,
#' returning column(s) and row(s)
#'
#' @param searchdata table to search in, predefined as rawdata
#' @param pattern regex, for exact matches add ^findme$
#' @param find.all return all row indices or only 1st per column,default=TRUE
#' @param names.only return only vector of colnames rather than list with names
#' and rows, default=FALSE
#'
#' @return A list with numeric vectors for each column giving row numbers
#' of matched elements
#' @export
tab.search <- function(searchdata = rawdata, pattern,
                       find.all = T, names.only = FALSE) {
  if (!is.character(pattern)) {
    pattern <- as.character(pattern)
  }
  positions <- purrr::map(searchdata, str_which, pattern = pattern) %>% purrr::compact()
  if (!find.all) {
    positions <- purrr::map(positions, nth, n = 1)
  }
  if (names.only) {
    positions <- names(positions)
  }
  return(positions)
}
