#' Pairwise Fisher's exact tests
#'
#' \code{pairwise_fisher_test} calculates pairwise comparisons between
#' group levels with corrections for multiple testing.
#'
#' @param dep_var dependent variable, containing the data.
#' @param indep_var independent variable, should be factor or coercible.
#' @param adjmethod method for adjusting p values (see [p.adjust]).
#' @param plevel threshold for significance.
#' @param symbols predefined as b,c, d...;  provides footnotes to mark group
#' differences, e.g. b means different from group 2
#' @param ref is the 1st subgroup the reference (like in Dunnett test)?
#'
#' @return
#' A list with elements "methods" (character), "p.value" (matrix),
#' "plevel" (numeric), and "sign_colwise" (vector of length number of levels - 1)
#'
#' @examples
#' # All pairwise comparisons
#' pairwise_fisher_test(dep_var = mtcars$cyl, indep_var = mtcars$gear)
#' # Only comparison against reference gear=3
#' pairwise_fisher_test(dep_var = mtcars$cyl, indep_var = mtcars$gear, ref = TRUE)
#' @export
pairwise_fisher_test <- function(dep_var, indep_var, adjmethod = "fdr", plevel = .05,
                                 symbols = letters[-1], # c('b','c','d','e','f','g'),
                                 ref = FALSE) {
  if (!is.factor(indep_var)) {
    indep_var <- factor(indep_var)
  }
  if (!is.factor(dep_var)) {
    dep_var <- factor(dep_var)
  }
  if (is.ordered(indep_var)) {
    indep_var <- factor(indep_var, ordered = F)
  }
  
  
  ngroups <- length(levels(indep_var))
  pft_data <- data.frame(dep_var, indep_var)
  pft_data <- na.omit(pft_data)
  # print(pft_data)
  p_unadj <- matrix(
    nrow = ngroups - 1, ncol = ngroups - 1,
    dimnames = list(c(2:ngroups), c(1:(ngroups - 1)))
  )
  for (firstgroup in 1:(ngroups - 1)) {
    for (secondgroup in (firstgroup + 1):ngroups) {
      tempdata <- pft_data[which(
        pft_data$indep_var == levels(pft_data$indep_var)[firstgroup] |
          pft_data$indep_var == levels(pft_data$indep_var)[secondgroup]
      ), ]
      if (min(dim(table(tempdata))) > 1) {
        p_unadj[secondgroup - 1, firstgroup] <-
          try(
            fisher.test(tempdata$dep_var, tempdata$indep_var,
                        simulate.p.value = T,B = 10^5)$p.value,
            silent = T)
      } else {
        p_unadj[secondgroup - 1, firstgroup] <- 1
      }
    }
  }
  # print(p_unadj)
  sign_colwise <- character()
  if (!ref) {
    p_adj <- matrix(p.adjust(as.vector(p_unadj), method = adjmethod),
                    byrow = FALSE,
                    nrow = ngroups - 1, ncol = ngroups - 1,
                    dimnames = list(c(2:ngroups), c(1:(ngroups - 1)))
    )
    for (col_i in 1:ncol(p_adj)) {
      temp <- " "
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i, col_i]) & p_adj[row_i, col_i] < plevel) {
          temp <- paste0(temp, symbols[row_i])
        }
      }
      sign_colwise <- c(sign_colwise, temp)
    }
  } else {
    p_adj <- p.adjust(as.vector(p_unadj[, 1]), method = adjmethod)
    sign_colwise <- markSign(p_adj) # sapply(p_adj,markSign)
  }
  return(list(
    method = adjmethod,
    p.value = p_adj,
    plevel = plevel,
    sign_colwise = sign_colwise
  ))
}

#' Pairwise comparison for ordinal categories
#'
#' \code{pairwise_ordcat_test} calculates pairwise comparisons for ordinal
#' categories between all group levels with corrections for multiple testing.
#'
#' @param dep_var dependent variable, containing the data
#' @param indep_var independent variable, should be factor
#' @param adjmethod method for adjusting p values (see [p.adjust])
#' @param plevel threshold for significance
#' @param symbols predefined as b,c, d...;  provides footnotes to mark group
#' differences, e.g. b means different from group 2
#' @param ref is the 1st subgroup the reference (like in Dunnett test)
#' @param cmh Should  Cochran-Mantel-Haenszel test (\link{cmh_test}) be used for
#' testing? If false, the linear-by-linear association test (\link{lbl_test})
#' is applied.
#'
#' @return
#' A list with elements "methods" (character), "p.value" (matrix),
#' "plevel" (numeric), and "sign_colwise" (vector of length number of levels - 1)
#'
#' @examples
#' # All pairwise comparisons
#' mtcars2 <- dplyr::mutate(mtcars, cyl = factor(cyl, ordered = TRUE))
#' pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear)
#' # Only comparison against reference gear=3
#' pairwise_ordcat_test(dep_var = mtcars2$cyl, indep_var = mtcars2$gear, ref = TRUE)
#' @export
pairwise_ordcat_test <- function(dep_var, indep_var, adjmethod = "fdr", plevel = .05,
                                 symbols = letters[-1],
                                 ref = FALSE, cmh = TRUE) {
  dep_var <- factor(dep_var, ordered = TRUE)
  indep_var <- factor(indep_var)
  ngroups <- length(levels(indep_var))
  pft_data <- data.frame(dep_var, indep_var)
  pft_data <- na.omit(pft_data)
  p_unadj <- matrix(
    nrow = ngroups - 1, ncol = ngroups - 1,
    dimnames = list(c(2:ngroups), c(1:(ngroups - 1)))
  )
  for (firstgroup in 1:(ngroups - 1)) {
    for (secondgroup in (firstgroup + 1):ngroups) {
      tempdata <- pft_data[which(
        as.numeric(pft_data$indep_var) %in% c(firstgroup, secondgroup)
      ), ]
      if (min(dim(table(tempdata))) > 1) {
        if (cmh) {
          # print('cmh_test')#(x~group,data=tempdata))
          p_unadj[secondgroup - 1, firstgroup] <-
            coin::pvalue(coin::cmh_test(dep_var ~ indep_var, data = tempdata))
        } else {
          # print('lbl_test')#(x~group,data=tempdata))
          p_unadj[secondgroup - 1, firstgroup] <-
            coin::pvalue(coin::lbl_test(dep_var ~ indep_var, data = tempdata))
        }
      } else {
        p_unadj[secondgroup - 1, firstgroup] <- 1
      }
    }
  }
  sign_colwise <- character()
  if (!ref) {
    p_adj <- matrix(p.adjust(as.vector(p_unadj), method = adjmethod),
                    byrow = FALSE,
                    nrow = ngroups - 1, ncol = ngroups - 1,
                    dimnames = list(c(2:ngroups), c(1:(ngroups - 1)))
    )
    for (col_i in 1:ncol(p_adj)) {
      temp <- " "
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i, col_i]) & p_adj[row_i, col_i] < plevel) {
          temp <- paste0(temp, symbols[row_i])
        }
      }
      sign_colwise <- c(sign_colwise, temp)
    }
  } else {
    p_adj <- p.adjust(as.vector(p_unadj[, 1]), method = adjmethod)
    sign_colwise <- sapply(p_adj, markSign)
  }
  return(list(
    method = adjmethod,
    p.value = p_adj,
    plevel = plevel,
    sign_colwise = sign_colwise,
    test = ifelse(cmh, "cmh", "lbl")
  ))
}

#' Kolmogorov-Smirnov-Test against Normal distribution
#'
#' \code{ksnormal} is a convenience function around \link{ks.test}, testing against
#' Normal distribution.
#' If less than 2 values are provided, NA is returned.
#'
#' @param x Vector of data to test.
#'
#' @return p.value from \link{ks.test}.
#'
#' @examples
#' # original ks.test:
#' ks.test(
#'   x = mtcars$wt, pnorm, mean = mean(mtcars$wt, na.rm = TRUE),
#'   sd = sd(mtcars$wt, na.rm = TRUE)
#' )
#' # wrapped version:
#' ksnormal(x = mtcars$wt)
#' @export
ksnormal <- function(x) {
  if(length(na.omit(x))>1){
  suppressWarnings(
    assign("ksout",ks.test(x, "pnorm", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
                     exact = FALSE
    )$p.value))
  }else{
    ksout <- NA
  }
  return(ksout)
}

#' Confidence interval for generalized linear models
#'
#' \code{glm_CI} computes and formats CIs for glm.
#'
#' @usage glmCI(model, min = .01, max = 100, cisep = '\U000022ef', ndigit=2)
#'
#' @return A list with coefficient, CIs, and pasted coef(\[CIs\]).
#'
#' @param model Output from \link{glm}.
#' @param min,max Lower and upper limits for CIs, useful for extremely wide CIs.
#' @param cisep Separator between CI values.
#' @param ndigit rounding level.
#'
#' @examples
#' glm_out <- glm(am ~ mpg, family = binomial, data = mtcars)
#' glmCI(glm_out)
#' @export
glmCI <- function(model, min = .01, max = 100, cisep = "\U000022ef", ndigit = 2) {
  glmReturn <- list(
    coeff = character(0), ci = character(0),
    c_ci = NA
  )
  pvTmp <- labels(model$terms)
  for (pv_i in 1:length(pvTmp))
  {
    rows <- grep(pvTmp[pv_i], names(model$coefficients))
    coeffTmp <- as.character(as.vector(round(exp(model$coefficients[rows]), ndigit)))
    coeffTmp[which(as.numeric(coeffTmp) < min)] <- paste0("<", min)
    coeffTmp[which(as.numeric(coeffTmp) > max)] <- paste0(">", max)
    glmReturn$coeff <- c(glmReturn$coeff, paste(coeffTmp, collapse = "/"))
    ciModel <- as.matrix(exp(confint(model)))
    ciTmp <- character(0)
    for (row_i in rows)
    {
      ciRow <- as.character(as.vector(round(ciModel[row_i, ], ndigit)))
      ciRow[which(as.numeric(ciRow) < min)] <- paste0("<", min)
      ciRow[which(as.numeric(ciRow) > max)] <- paste0(">", max)
      ciTmp <- paste(ciTmp, paste(ciRow, collapse = cisep),
                     sep = "/"
      )
    }
    glmReturn$ci <- c(glmReturn$ci, gsub("^/", "", ciTmp))
  }
  glmReturn$c_ci <- paste0(glmReturn$coeff, " (", glmReturn$ci, ")")
  return(glmReturn)
}

#' Correlations with significance
#'
#' \code{cortestR} computes correlations and their significance level
#' based on \link{cor.test}. Coefficients and p-values may be combined or
#' reported separately.
#'
#' @return Depending on parameters split and sign_symbol,
#' either a single data frame with coefficient and p-values or significance symbols
#' or a list with two data frames.
#'
#' @param cordata data frame or matrix with rawdata.
#' @param method as in cor.test.
#' @param digits rounding level for estimate.
#' @param digits_p rounding level for p value.
#' @param sign_symbol If true, use significance indicator instead of p-value.
#' @param split logical, report correlation and p combined (default)
#' or split in list.
#' @param space character to fill empty upper triangle.
#' @examples
#' # with defaults
#' cortestR(mtcars[, c("wt", "mpg", "qsec")], split = FALSE, sign_symbol = TRUE)
#' # separate coefficients and p-values
#' cortestR(mtcars[, c("wt", "mpg", "qsec")], split = TRUE, sign_symbol = FALSE)
#' @export
cortestR <- function(cordata, method = "pearson",
                     digits = 3, digits_p = 3,
                     sign_symbol = TRUE,
                     split = FALSE,
                     space = "") {
  if(!is.matrix(cordata)){
    cordata <- as.matrix(cordata)
  }
  n <- ncol(cordata)
  corout <- as.data.frame(
    matrix(space, nrow = n, ncol = n)
  )
  colnames(corout) <- colnames(cordata)
  rownames(corout) <- colnames(corout)
  if (split) {
    pout <- corout
  }
  for (row_i in 1:n) {
    for (col_i in 1:row_i) {
      ct <- cor.test(cordata[, row_i], cordata[, col_i],
                     method = method
      )
      corout[row_i, col_i] <-
        round(ct$estimate, digits)
      if (!split) {
        if (row_i != col_i) {
          if (sign_symbol) {
            corout[row_i, col_i] <- paste0(
              corout[row_i, col_i],
              markSign(ct$p.value)
            )
          } else {
            corout[row_i, col_i] <- paste0(
              corout[row_i, col_i], " (",
              formatP(ct$p.value, ndigits = digits_p),
              ")"
            )
          }
        }
      } else {
        if (sign_symbol) {
          pout[row_i, col_i] <- markSign(ct$p.value)  |>  as.character()
        } else {
          pout[row_i, col_i] <- formatP(ct$p.value)
        }
      }
    } 
  }
  if (split) {
    return(list(
      corout = corout,
      pout = pout
    ))
  } else {
    return(corout)
  }
}

#' Independent sample t-test with test for equal variance
#'
#' \code{t_var_test} tests for equal variance based on \link{var.test}
#' and calls t.test, setting the option var.equal accordingly.
#'
#' @param data Tibble or data_frame.
#' @param formula Formula object with dependent and independent variable.
#' @param cutoff is significance threshold for equal variances.
#'
#' @return
#' A list from \link{t.test}
#'
#' @examples
#' t_var_test(mtcars, wt ~ am)
#' # may be used in pipes:
#' mtcars |> t_var_test(wt ~ am)
#' @export
t_var_test <- function(data, formula, cutoff = .05) {
  formula <- as.formula(formula)
  t_out <- ""
  var.equal <- try(
    var.test(
      formula = formula,
      data = data
    )$p.value > cutoff,
    silent = TRUE
  )
  if (is.logical(var.equal)) {
    t_out <- t.test(
      formula = formula, data = data,
      var.equal = var.equal
    )
  }
  return(t_out)
}

#' Comparison for columns of numbers for 2 groups
#'
#' \code{compare2numvars} computes either \link{t_var_test} or \link{wilcox.test},
#' depending on parameter gaussian. Descriptive statistics, depending on distribution,
#' are reported as well.
#'
#' @param data name of dataset (tibble/data.frame) to analyze.
#' @param dep_vars vector of column names for independent variables.
#' @param indep_var name of grouping variable, has to translate to 2 groups. If more levels are encountered, an error is produced.
#' @param gaussian logical specifying normal or ordinal values.
#' @param round_p level for rounding p-value.
#' @param round_desc number of significant digits for rounding of descriptive stats.
#' @param range include min/max?
#' @param rangesep text between statistics and range or other elements.
#' @param pretext for function [formatP].
#' @param mark for function [formatP].
#' @param n create columns for n per group?
#' @param add_n add n to descriptive statistics?
#'
#' @return
#' A tibble with variable names, descriptive statistics, and p-value,
#' number of rows is number of dep_vars.
#'
#' @examples
#' # Assuming Normal distribution:
#' compare2numvars(
#'   data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
#'   gaussian = TRUE
#' )
#' # Ordinal scale:
#' compare2numvars(
#'   data = mtcars, dep_vars = c("wt", "mpg", "qsec"), indep_var = "am",
#'   gaussian = FALSE
#' )
#' # If dependent variable has more than 2 levels, consider fct_lump:
#' mtcars |> dplyr::mutate(gear=factor(gear) |> forcats::fct_lump_n(n=1)) |> 
#' compare2numvars(dep_vars="wt",indep_var="gear",gaussian=TRUE)
#' 
#' @export
compare2numvars <- function(data, dep_vars, indep_var,
                            gaussian, round_p = 3, round_desc = 2,
                            range = FALSE,
                            rangesep = " ",
                            pretext = FALSE, mark = FALSE, 
                            n = FALSE, add_n = FALSE) {
  `.` <- Group <- Value <- Variable <- desc_groups <- NULL
  if (gaussian) {
    DESC <- meansd
    COMP <- t_var_test
  } else {
    DESC <- median_quart
    COMP <- wilcox.test
  }
  # descnames <- names(formals(DESC))
  # pnames <- names(formals(COMP))
  
  data_l <- data |>
    dplyr::select(
      Group = all_of(indep_var),
      all_of(dep_vars)
    ) |>
    mutate(Group = factor(Group) |> fct_drop()) |>
    pivot_longer(-Group,names_to = 'Variable',values_to = 'Value') |> 
    # gather(key = Variable, value = Value, -Group) |>
    mutate(Variable = forcats::fct_inorder(Variable)) |>
    # na.omit() |>
    as_tibble()
  if(nlevels(data_l$Group)!=2){
    stop(paste0('Other than 2 groups provided for ',indep_var,': ',
                paste(levels(data_l$Group),collapse='/'),
                ". Look into function compare_n_numvars."))
  }
  data_l <- data_l |> 
    filter(!is.na(Group))
  out <- data_l |>
    group_by(Variable) |>
    do(summarise(
      .data = .,
      n_groups = paste(table(.$Group[which(!is.na(.$Value))]), collapse = ":"),
      desc_all = DESC(.$Value,
                      roundDig = round_desc,
                      range = range, rangesep = rangesep,
                      add_n = add_n
      ),
      desc_groups = paste(try(
        DESC(
          x = .$Value, groupvar = .$Group,
          roundDig = round_desc, range = range,
          rangesep = rangesep, add_n = add_n
        ),
        silent = TRUE
      ),
      collapse = ":"
      ),
      p = formatP(try(
        suppressWarnings(COMP(formula = as.formula("Value~Group"), data = .)$p.value),
        silent = TRUE
      ),
      ndigits = round_p, pretext = pretext,
      mark = mark
      ) |> as.character()
    )) |> 
    ungroup()
  out$desc_groups[!str_detect(out$desc_groups, ":")] <- " : "
  out <- separate(out,
                  col = desc_groups,
                  into = glue::glue("{indep_var} {levels(data_l$Group)}"),
                  sep = ":"
  )
  out <- separate(out,
                  col = n_groups,
                  into = glue::glue("n {indep_var} {levels(data_l$Group)}"),
                  sep = ":"
  )
  out$n <- apply(out[, 2:3], 1, function(x) {
    sum(as.numeric(x))
  })
  out <- out |> dplyr::select(1, n, starts_with("n "), everything())
  
  if (n == FALSE) {
    out <- dplyr::select(out, -n, -starts_with("n "))
  }
  return(out)
}

#' Comparison for columns of factors for 2 groups
#'
#' \code{compare2qualvars} computes \link{fisher.test} with simulated p-value and
#' descriptive statistics for a group of categorical dependent variables.
#'
#' @param data name of data set (tibble/data.frame) to analyze.
#' @param dep_vars vector of column names for dependent variables.
#' @param indep_var name of grouping variable, has to translate to 2 groups.
#' @param round_p level for rounding p-value.
#' @param round_desc number of significant digits for rounding of descriptive stats.
#' @param pretext for function [formatP].
#' @param mark for function [formatP].
#' @param singleline Put all group levels in  a single line?
#' @param spacer Text element to indent levels and fill empty cells,
#' defaults to " ".
#' @param linebreak place holder for newline.
#' @param p_subgroups test subgroups by recoding other levels into other, default is not to do this.
#'
#' @return
#' A tibble with variable names, descriptive statistics, and p-value,
#' number of rows is number of dep_vars.
#'
#' @examples
#' compare2qualvars(
#'   data = mtcars, dep_vars = c("gear", "cyl", "carb"), indep_var = "am",
#'   spacer = " "
#' )
#' compare2qualvars(
#'   data = mtcars, dep_vars = c("gear", "cyl", "carb"), indep_var = "am",
#'   spacer = " ", singleline = TRUE
#' )
#' compare2qualvars(
#'   data = mtcars, dep_vars = c("gear", "cyl", "carb"), indep_var = "am",
#'   spacer = " ", p_subgroups = TRUE
#' )
#' @export
compare2qualvars <- function(data, dep_vars, indep_var,
                             round_p = 3, round_desc = 2,
                             pretext = FALSE, mark = FALSE,
                             singleline = FALSE,
                             # newline=TRUE,
                             spacer = " ",
                             linebreak = "\n",
                             p_subgroups = FALSE) {
  indentor <- paste0(rep(spacer, 5), collapse = "")
  if (!(is.factor(data |> pull(indep_var)))) {
    data <- data |> mutate(!!indep_var := factor(!!sym(indep_var)))
  }
  if(data |> pull(indep_var) |> nlevels() !=2){
    stop(paste("Independent variable",indep_var,
               "has",data |> pull(indep_var) |> nlevels(),
               "levels but must have exactly 2.",
               "Look into function compare_n_qualvars."))
  }
  for(var_i in dep_vars){
    if (!(is.factor(data |> pull(var_i)))) {
      data <- data |> mutate(!!var_i := factor(!!sym(var_i)))
    }
    
  }
  freq <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(
                   x,
                   return_level = FALSE, singleline = singleline,
                   ndigit = round_desc
                 )
               }
    ) |>
    purrr::map(as_tibble)
  
  
  levels <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(x,
                                singleline = singleline
                 )$level
               }
    ) |>
    purrr::map(as_tibble)
  freqBYgroup <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(x,
                                groupvar = data[[indep_var]],
                                return_level = FALSE,
                                ndigit = round_desc,
                                singleline = singleline
                 )
               }
    )
  
  p <-
    purrr::map2(data[dep_vars], data[indep_var],
                .f = function(x, y) {
                  try(formatP(try(
                    fisher.test(
                      x = x, y = y,
                      simulate.p.value = TRUE,
                      B = 10^5
                    )$p.value,
                    silent = TRUE
                  ),
                  mark = mark, pretext = pretext
                  ),silent=TRUE) |> 
                    as.character()
                }
    ) |> 
    purrr::map(~case_when(str_detect(.,'.\\d+') ~ .,TRUE~''))
  
  if(p_subgroups){
    for(var_i in dep_vars){
      freqBYgroup[[var_i]]$p <- NA_character_
      subgroups=data |> pull(var_i) |> levels()
      for(sg_i in seq_along(subgroups)){
        testdata <- 
          data |> 
          select(all_of(c(indep_var,var_i))) |> 
          mutate(testvar=forcats::fct_collapse(!!sym(var_i),
                                      check=subgroups[sg_i],
                                      other_level = 'other')) |> 
          select(all_of(indep_var),'testvar') |> table()
        if(ncol(testdata)>1) {
          p_sg <- fisher.test(testdata,
                            simulate.p.value = TRUE,
                            B = 10^5)$p.value |> 
          formatP(mark = mark, pretext = pretext)
        } else{
          p_sg <- ''
        }
        if(singleline){
          freqBYgroup[[var_i]]$p <- 
            paste(na.omit(freqBYgroup[[var_i]]$p),p_sg) |> 
            str_squish()} else {
        freqBYgroup[[var_i]]$p[sg_i] <- p_sg
            }
      }
    }
  }  
  
  out <- tibble(
    Variable = character(), desc_all = character(),
    g1 = character(), g2 = character(), p = character()
  )
  if(p_subgroups){
    out$pSubgroup <- NA_character_
  }
  for (var_i in seq_along(dep_vars)) {
    if (!singleline) {
      out_tmp <- add_row(out[0,],
                     Variable = c(
                       dep_vars[var_i],
                       glue::glue(
                         "{indentor}{levels[[var_i]][[1]]}"
                       )
                     ),
                     desc_all = c(spacer, freq[[var_i]][[1]]),
                     g1 = c(spacer, freqBYgroup[[var_i]][[1]]),
                     g2 = c(spacer, freqBYgroup[[var_i]][[2]]),
                     p = c(
                       p[[var_i]][[1]],
                       rep(spacer, nrow(freqBYgroup[[var_i]]))
                     )
      )
      if(p_subgroups){
        out_tmp$pSubgroup <- c(spacer,freqBYgroup[[var_i]]$p)
      }     
      out <- rbind(out,out_tmp)
    } else {
      out_tmp <- add_row(out[0,],
                     Variable = paste(
                       dep_vars[var_i],
                       # rep(spacer,
                       #     nrow(freqBYgroup[[var_i]])-1)),
                       levels[[var_i]][[1]]
                     ),
                     desc_all = freq[[var_i]][[1]],
                     g1 = freqBYgroup[[var_i]][[1]],
                     g2 = freqBYgroup[[var_i]][[2]],
                     p = p[[var_i]][[1]]
      )
      if(p_subgroups){
        out_tmp$pSubgroup <- freqBYgroup[[var_i]]$p
      }     
      out <- rbind(out,out_tmp)
    }
  }
  colnames(out) <- colnames(out) |> str_replace_all(
    c(
      "g1" = paste(
        indep_var,
        data |> pull(indep_var) |> levels() |> first()
      ),
      "g2" = paste(
        indep_var,
        data |> pull(indep_var) |> levels() |> last()
      )
    )
  )
  return(out)
}

#' Comparison for columns of factors for more than 2 groups with post-hoc
#' @param data name of data set (tibble/data.frame) to analyze.
#' @param dep_vars vector of column names.
#' @param indep_var name of grouping variable.
#' @param round_p level for rounding p-value.
#' @param round_desc number of significant digits for rounding of descriptive stats
#' @param pretext for function [formatP]
#' @param mark for function [formatP]
#' @param singleline Put all group levels in  a single line?
#' @param spacer Text element to indent levels, defaults to " ".
#' @param linebreak place holder for newline.
#' @param prettynum Apply prettyNum to results?
#'
#' @return
#' A tibble with variable names, descriptive statistics, and p-value of \link{fisher.test}
#' and \link{pairwise_fisher_test}, number of rows is number of dep_vars.
#'
#' @examples
#' # Separate lines for each factor level:
#' compare_n_qualvars(
#'   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#'   spacer = " "
#' )
#' # All levels in one row but with linebreaks:
#' compare_n_qualvars(
#'   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#'   singleline = TRUE
#' )
#' # All levels in one row, separateted by ";":
#' compare_n_qualvars(
#'   data = mtcars, dep_vars = c("am", "cyl", "carb"), indep_var = "gear",
#'   singleline = TRUE, linebreak = "; "
#' )
#' @export
compare_n_qualvars <- function(data, dep_vars, indep_var,
                               round_p = 3, round_desc = 2,
                               pretext = FALSE, mark = FALSE,
                               singleline = FALSE,
                               # newline=TRUE,
                               spacer = " ",
                               linebreak = "\n",
                               prettynum = FALSE) {
  indentor <- paste0(rep(spacer, 5), collapse = "")
  
  if (!(is.factor(data |> pull(indep_var)))) {
    data <- data |> mutate(!!indep_var := factor(!!sym(indep_var)))
  }
  # groups <- levels(data[[indep_var]])
  freq <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(
                   x,
                   return_level = FALSE, singleline = singleline,
                   ndigit = round_desc, separator = linebreak,
                   prettynum = prettynum
                 )
               }
    ) |>
    purrr::map(as_tibble)
  
  
  levels <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(x,
                                singleline = singleline,
                                separator = linebreak
                 )$level
               }
    ) |>
    purrr::map(as_tibble)
  freqBYgroup <-
    purrr::map(data[dep_vars],
               .f = function(x) {
                 cat_desc_stats(x,
                                groupvar = data[[indep_var]],
                                return_level = FALSE,
                                ndigit = round_desc,
                                singleline = singleline,
                                separator = linebreak,
                                prettynum = prettynum
                 )
               }
    )
  
  p <-
    purrr::map2(data[dep_vars], data[indep_var],
                .f = function(x, y) {
                  try(
                    formatP(
                      try(
                        fisher.test(
                          x = x, y = y, simulate.p.value = TRUE,
                          B = 10^4
                        )$p.value,
                        silent = TRUE
                      ),
                      mark = mark, pretext = pretext),
                    silent=T) |> 
                    tidyr::replace_na('')
                  
                }
    )
  
  out <- tibble(Variable = character(), desc_all = character()) |>
    left_join(freqBYgroup[[1]] |> slice(0), by = character()) |>
    mutate(p = character())
  out_template <- out
  groupcols <- 3:(ncol(out) - 1)
  for (var_i in seq_along(dep_vars)) {
    testdata <- data |>
      dplyr::select(all_of(c(indep_var, dep_vars[var_i]))) |>
      na.omit()
    pairwise_p <- 
      pairwise_fisher_test(testdata[[2]], testdata[[1]])$sign_colwise |>
      str_replace("^ $", spacer)
    if (!singleline) {
      out_tmp <- add_row(out_template,
                         Variable = c(
                           dep_vars[var_i],
                           str_glue(
                             "{indentor}{levels[[var_i]][[1]]}"
                           )
                         ),
                         desc_all = c(spacer, freq[[var_i]][[1]])
      )
      out_tmp[1, groupcols] <- c(pairwise_p, spacer) |> as.list()
      out_tmp[-1, groupcols] <- freqBYgroup[[var_i]]
      out_tmp["p"] <- c(
        p[[var_i]][[1]],
        rep(spacer, nrow(freqBYgroup[[var_i]]))
      )
    } else {
      out_tmp <- add_row(out_template,
                         Variable = paste(
                           dep_vars[var_i],
                           # rep(spacer,
                           # nrow(freqBYgroup[[var_i]])-1),
                           levels[[var_i]][[1]]
                         ),
                         desc_all = freq[[var_i]][[1]]
      )
      out_tmp[1, groupcols] <- paste(freqBYgroup[[var_i]], c(pairwise_p, spacer)) |>
        as.list()
      out_tmp["p"] <- p[[var_i]]
    }
    out <- out |> rbind(out_tmp)
  }
  return(out)
}


#' Pairwise Wilcoxon tests
#'
#' \code{pairwise_wilcox_test} calculates pairwise comparisons on ordinal data
#' between all group levels with corrections for multiple testing based on
#' \link{wilcox_test} from package 'coin'.
#'
#' @param dep_var dependent variable, containing the data.
#' @param indep_var independent variable, should be factor.
#' @param strat_var optional factor for stratification.
#' @param adjmethod method for adjusting p values (see [p.adjust])
#' @param distr Computation of p-values, see \link{wilcox_test}.
#' @param plevel threshold for significance.
#' @param symbols predefined as b,c, d...;  provides footnotes to mark group
#' differences, e.g. b means different from group 2.
#' @param sep text between statistics and range or other elements.
#'
#' @return
#' A list with matrix of adjusted p-values and character vector with significance indicators.
#' @examples
#' pairwise_wilcox_test(dep_var = mtcars$wt, indep_var = mtcars$cyl)
#' @export
pairwise_wilcox_test <-
  function(dep_var, indep_var, strat_var = NA,
           adjmethod = "fdr", distr = "exact", plevel = .05,
           symbols = letters[-1],
           sep = "") {
    # if (!is.factor(indep_var))
    # {
    indep_var <- factor(indep_var)
    # }
    ngroups <- length(levels(indep_var))
    if (length(strat_var) == 1) {
      strat_var <- rep(1, length(dep_var))
    }
    if (!is.factor(strat_var)) {
      strat_var <- factor(strat_var)
    }
    pwt_data <- tibble(dep_var,
                       indep_var = as.numeric(indep_var),
                       strat_var
    )
    p_unadj <- matrix(
      nrow = ngroups - 1, ncol = ngroups - 1,
      dimnames = list(c(2:ngroups), c(1:(ngroups - 1)))
    )
    for (firstgroup in 1:(ngroups - 1)) {
      for (secondgroup in (firstgroup + 1):ngroups) {
        tempdata <- pwt_data[c(
          which(pwt_data$indep_var == firstgroup),
          which(pwt_data$indep_var == secondgroup)
        ), ]
        tempdata$indep_var <- factor(tempdata$indep_var)
        # print(tempdata)
        if (length(levels(as.factor(tempdata$dep_var))) > 1) {
          p_unadj[secondgroup - 1, firstgroup] <-
            coin::pvalue(coin::wilcox_test(tempdata$dep_var ~ tempdata$indep_var |
                                             tempdata$strat_var,
                                           distribution = distr
            ))
        }
        else {
          p_unadj[secondgroup - 1, firstgroup] <- 1
        }
      }
    }
    p_adj <- matrix(p.adjust(as.vector(p_unadj), method = adjmethod),
                    byrow = FALSE,
                    nrow = ngroups - 1, ncol = ngroups - 1,
                    dimnames = list(
                      group2 = levels(indep_var)[-1],
                      group1 = levels(indep_var)[-ngroups]
                    )
    )
    sign_colwise <- character()
    for (col_i in 1:ncol(p_adj)) {
      temp <- " "
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i, col_i]) &
            p_adj[row_i, col_i] < plevel) {
          temp <- paste(temp, symbols[row_i], sep = sep)
        }
      }
      sign_colwise <- c(sign_colwise, temp)
    }
    
    return(list(
      p_adj = p_adj,
      sign_colwise = sign_colwise
    ))
  }

#' Extended pairwise t-test
#'
#' \code{pairwise_t_test}calculate pairwise comparisons between group levels
#' with corrections for multiple testing based on \link{pairwise.t.test}
#'
#' @param dep_var dependent variable, containing the data
#' @param indep_var independent variable, should be factor
#' @param adjmethod method for adjusting p values (see [p.adjust])
#' @param plevel threshold for significance
#' @param symbols predefined as b,c, d...;  provides footnotes to mark group
#' differences, e.g. b means different from group 2
#' @return
#' A list with method output of pairwise.t.test,
#' matrix of p-values, and character vector with significance indicators.
#' @examples
#' pairwise_t_test(dep_var = mtcars$wt, indep_var = mtcars$cyl)
#' @export
pairwise_t_test <- function(dep_var, indep_var, adjmethod = "fdr", plevel = .05,
                            symbols = letters[-1]) {
  t_out <- pairwise.t.test(x = dep_var, g = indep_var, p.adjust.method = adjmethod)
  p_colwise <- t_out$p.value
  sign_colwise <- character()
  for (col_i in 1:ncol(p_colwise)) {
    temp <- " "
    for (row_i in col_i:nrow(p_colwise)) {
      if (!is.na(p_colwise[row_i, col_i]) &
          p_colwise[row_i, col_i] < plevel) {
        temp <- paste0(temp, symbols[row_i])
      }
    }
    sign_colwise <- c(sign_colwise, temp)
  }
  return(list(
    method = t_out$method,
    p.value = t_out$p.value,
    plevel = plevel,
    sign_colwise = sign_colwise
  ))
}


#' Comparison for columns of Gaussian or ordinal measures for n groups
#'
#' @description Some names were changed in August 2022, to reflect the update of the function to handle ordinal data using non-parametric equivalents.
#'
#' @param .data name of dataset (tibble/data.frame) to analyze, defaults to rawdata.
#' @param dep_vars vector of column names.
#' @param indep_var name of grouping variable.
#' @param gaussian Logical specifying normal or ordinal indep_var (and chooses comparison tests accordingly) 
#' @param round_p level for rounding p-value.
#' @param round_desc number of significant digits for rounding of descriptive stats.
#' @param range include min/max?
#' @param rangesep text between statistics and range or other elements.
#' @param pretext,mark for function formatP.
#' @param add_n add n to descriptive statistics?
#'
#' @return
#' A list with elements "results": tibble with descriptive statistics,
#' p-value from ANOVA/Kruskal-Wallis test, p-values for pairwise comparisons, significance 
#' indicators, and descriptives pasted with significance.
#' "raw": nested list with output from all underlying analyses.
#'
#' @examples
#' # Usually,only the result table is relevant:
#' compare_n_numvars(
#'   .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
#'   indep_var = "drat",
#'   gaussian = TRUE
#' )$results
#' # For a report, result columns may be filtered as needed:
#' compare_n_numvars(
#'   .data = mtcars, dep_vars = c("wt", "mpg", "hp"),
#'   indep_var = "cyl",
#'   gaussian = FALSE
#' )$results |>
#'   dplyr::select(Variable, `cyl 4 fn`:`cyl 8 fn`, multivar_p)
#' @export
compare_n_numvars <- function(.data = rawdata,
                              dep_vars, indep_var, gaussian,
                              round_desc = 2, range = FALSE,
                              rangesep = " ",
                              pretext = FALSE, mark = FALSE, round_p = 3,
                              add_n = FALSE) {
  value <- Variable <- lm_out <- p_tout <- pANOVA <- NULL
  if(gaussian){
    desc_fun <- wrappedtools::meansd
    grptest <- stats::lm
  } else {
    desc_fun <- wrappedtools::median_quart
    grptest <- stats::kruskal.test
  }
  # if (gaussian) {
  if (!is.factor(.data[[indep_var]]) |
      is.ordered(.data[[indep_var]])) {
    .data[[indep_var]] <- factor(.data[[indep_var]],
                                 ordered = FALSE
    )}
  glevel <- forcats::fct_inorder(levels(.data[[indep_var]]))
  .data <- dplyr::select(
    .data, all_of(dep_vars),
    all_of(indep_var)
  )
  t <- .data |>
    tidyr::pivot_longer(
      cols = all_of(dep_vars),
      values_to = "value", names_to = "Variable"
    ) |>
    nest(data = c(all_of(indep_var), value)) |>
    mutate(
      Variable = forcats::fct_inorder(as.factor(Variable)),
      desc_tab = purrr::map_chr(data, ~ desc_fun(.$value,
                                                 roundDig = round_desc,
                                                 range = range,
                                                 rangesep = rangesep,
                                                 add_n = add_n
      )),
      desc_grp = purrr::map(data, ~ desc_fun(.$value,
                                             groupvar = .[indep_var],
                                             roundDig = round_desc,
                                             range = range,
                                             rangesep = rangesep,
                                             add_n = add_n
      )) |>
        purrr::map(~ set_names(
          .x,
          as.character(glevel)
        )),
      lm_out = if (gaussian) {
        purrr::map(data, ~ stats::lm(value ~ !!sym(indep_var), data = .x))},
      anova_out= if (gaussian){purrr::map(lm_out, anova)} else {
        purrr::map(data, ~ stats::kruskal.test(value ~ !!sym(indep_var), data = .x))
      },
      `p_wcox/t_out` = if (gaussian) {
        purrr::map(data, ~ pairwise.t.test(.x[["value"]],
                                           g = .x[[indep_var]],
                                           pool.sd = TRUE,
                                           p.adjust.method = "none"
        )$p.value)} else {
          purrr::map(data, ~ pairwise.wilcox.test(.x[["value"]],
                                                  g = .x[[indep_var]],
                                                  p.adjust.method= "none",
                                                  exact = FALSE)$p.value)
        },
      p_wcox_t_out = if (gaussian) {
        purrr::map(data, ~ pairwise_t_test(
          .x[["value"]],
          .x[[indep_var]]
        )$sign_colwise)} else {
          purrr::map(data, ~ pairwise_wilcox_test(
            .x[["value"]],
            .x[[indep_var]],
            distr = "as"
          )$sign_colwise)},
      p_wcox_t_out = purrr::map(p_wcox_t_out, ~ c(.x,
                                                  "")) #add empty string for last column
    ) |>
    purrr::map(~ set_names(.x, dep_vars))
  
  
  p_results <- NULL
  if (gaussian) {
    p_results <- "Pr(>F)"
  } else {
    p_results <- "p.value"
  }
  multivar_p <- NULL
  if (gaussian) {
    multivar_p <- "pANOVA"
  } else {
    multivar_p <- "pKW"
  }
  results <- NULL
  results <- tibble(Variable = forcats::fct_inorder(dep_vars), all = t$desc_tab) |>
    full_join(purrr::reduce(t$desc_grp, rbind) |>
                matrix(nrow = length(dep_vars), byrow = FALSE) |>
                as_tibble(.name_repair = "unique") |>
                mutate(Variable = dep_vars) |>
                dplyr::select(Variable, everything()) |>
                set_names(c(
                  "Variable",
                  paste(indep_var, glevel)
                ))) |>
    full_join(purrr::map_df(t$anova_out, p_results) |> slice(1) |>
                pivot_longer(everything(),names_to = 'Variable', 
                             values_to = 'multivar_p') |> 
                # gather(key = "Variable", value = multivar_p)|>
                mutate(Variable = forcats::fct_inorder(Variable)) |>
                mutate(multivar_p = formatP(multivar_p,
                                            ndigits = round_p,
                                            pretext = pretext,
                                            mark = mark))) |> #as.vector()|>
    full_join(purrr::map_df(t$`p_wcox/t_out`, ~ paste(formatP(
      p.adjust(.x[lower.tri(.x, TRUE)], method = "fdr")),
      collapse = ";")) |>
        pivot_longer(everything(),names_to = 'Variable', 
                     values_to = 'p between groups')) |> 
        # gather(key = "Variable", value = "p between groups")) |>
    full_join(purrr::reduce(t$p_wcox_t_out, rbind) |>
                matrix(nrow = length(dep_vars), byrow = FALSE) |>
                as_tibble(.name_repair = "unique") |>
                mutate(Variable = dep_vars) |>
                set_names(c(paste("sign", glevel), "Variable"))) |>
    full_join(purrr::map_df(t$`p_wcox/t_out`, ~ paste(formatP(p.adjust(.x[, 1],
                                                                       method = "fdr"
    )),
    collapse = ";"
    )) |>
      pivot_longer(everything(),names_to = 'Variable', 
                   values_to = 'p vs.ref'))  
      # gather(key = "Variable", value = "p vs.ref"))
  results <- cbind(
    results,
    purrr::map2_df(
      .x = dplyr::select(results, starts_with(indep_var)),
      .y = dplyr::select(results, starts_with("sign")),
      .f = paste
    ) |>
      rename_all(paste, "fn")
  ) |>
    as_tibble(.name_repair = "unique")
  # todo: p vs. ref symbol
  return(
    list(results = results,
         raw = t))
}
utils::globalVariables('p_wcox_t_out')

