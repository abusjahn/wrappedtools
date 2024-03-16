#' Comparison for groups in clinical trials based on all possible combinations of subjects
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' \code{WINratio} computes the ratio of wins and losses for any number
#' of comparison rules.
#'
#' @param data name of data set (tibble/data.frame) to analyze.
#' @param groupvar name of grouping variable, has to translate to 2 groups.
#' @param testvars names of variables for sequential rules.
#' @param rules list of rules (minimal cut-offs) for sequential comparison, negative if reduction is success, positive if increase is beneficial, must not be 0.
#' @param idvar name of identifier variable. If NULL, rownumber is used.
#' @param p_digits level for rounding p-value.
#'
#' @return
#' A list with elements:
#' 
#' WINratio=vector with WINratio and CIs,
#' 
#' WINodds=odds ratio of wins and losses, taking ties into account,
#' 
#' p.value=p.value from prop.test,
#' 
#' WINratioCI=character with merged WINratio, CI, and p
#' 
#' testdata= tibble with testdata from cross-join.
#' 
#' @export
#' 
WINratio <- function(data,groupvar,testvars,rules, idvar=NULL,
                     p_digits=3){
  if(any(rules==0)){
    stop("rules must not be 0 but give direction and magnitude of minimal difference")
  }
  data <- select(data,any_of(c(groupvar,testvars,idvar)))
  if((!is.factor(data[[groupvar]]))){
    data[[groupvar]] <- factor(data[[groupvar]])
  }
  if(nlevels(data[[groupvar]])!=2){
    stop("groupvar must have exactly two levels")
  }
  groupvar_level <- levels(data[[groupvar]])
  grp1 <- data |>filter(!!sym(groupvar)==groupvar_level[1])
  colnames(grp1) <- c("GRP",paste0("X",seq_along(testvars)))
  grp2 <- data |>filter(!!sym(groupvar)==groupvar_level[2])
  colnames(grp2) <- c("GRP",paste0("Y",seq_along(testvars)))
  
  testdata <- cross_join(grp1,grp2) |>
    mutate(WIN=0)
  for(rule_i in seq_along(rules)){
    testdata <-
      testdata |>
      rowwise() |>
      mutate(
        !!sym(paste0("rule",rule_i,"out")) :=
          case_when(
            #already decided
            WIN!=0 ~ NA_integer_,
            #sign of delta=sign of rule and win
            abs(!!sym(paste0("X",rule_i))-
                  !!sym(paste0("Y",rule_i)))>=abs(rules[rule_i]) &
              (sign(!!sym(paste0("X",rule_i))-
                      !!sym(paste0("Y",rule_i)))==sign(rules[rule_i])) ~1,
            #sign of delta!=sign of rule and loose
            abs(!!sym(paste0("X",rule_i))-
                  !!sym(paste0("Y",rule_i)))>=abs(rules[rule_i]) &
              (sign(!!sym(paste0("X",rule_i))-
                      !!sym(paste0("Y",rule_i)))!=sign(rules[rule_i])) ~-1,
            .default=0),
        WIN=sum(c_across(starts_with("rule")),na.rm=TRUE)
      ) |>
      ungroup()
  }
  
  WINners <-
    testdata |>
    # group_by() |>
    summarize(
      across(starts_with("rule"),
             list(Wins=~sum(.x==1,na.rm=TRUE),
                  Losses=~sum(.x==-1,na.rm=TRUE),
                  Ties=~sum(.x==0,na.rm=TRUE),
                  NC=~sum(is.na(.x))))) |>
    pivot_longer(everything(),
                 names_to=c("rule","outcome"),
                 names_sep = "_") |>
    pivot_wider(names_from = outcome) |>
    add_row(rule="all") |>
    mutate(across(-rule,
                  ~case_when(rule=="all" ~sum(.x, na.rm=TRUE),
                             .default=.x))) |>
    mutate(rule=c(paste(testvars,rules, sep=": "),"all"))
  
  p_w <- WINners |>
    filter(rule=="all") |>
    pull("Wins")# / nrow(testdata)
  p_l <- WINners |>
    filter(rule=="all") |>
    pull("Losses")# / nrow(testdata)
  p_t <- WINners |>
    filter(rule=="all") |>
    pull("Ties")# / nrow(testdata)
  # WINratio=pT/pC
  p.value <- prop.test(p_w,p_w+p_l)$p.value
  WINratio <- DescTools::BinomRatioCI(p_w,nrow(testdata),
                                      p_l,nrow(testdata),
                                      method = 'katz') |>
    roundR(3)
  WINodds <- (p_w+p_t*.5) / (p_l+p_t*.5)
  WINratioCI <- paste0(roundR(WINratio[1],3)," (",
                       roundR(WINratio[2],3),"/",
                       roundR(WINratio[3],3),")",
                       " p ",formatP(p.value, ndigits=p_digits, pretext=TRUE))
  return(list(WIN=WINners,
              WINratio=WINratio,
              WINodds=WINodds,
              p.value=p.value,
              WINratioCI=WINratioCI,
              testdata=testdata))
}
utils::globalVariables(c('outcome',"rule"))

#' Estimation of glomerular filtration rate (eGFR) based on sex, age, and either serum creatinine and/or cystatin C
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' \code{eGFR} computes eGFR according to different rules (see references).
#'
#' @param data name of data set (tibble/data.frame) to analyze.
#' @param age_var name of column with patient age in years, default=age.
#' @param sex_var name of column with sex, assumed as female and male.
#' @param crea_var name of column with creatinine in mg/dl. If not available, leave as NULL.
#' @param cys_var name of column with cystatin C in mg/l. If not available, leave as NULL.
#'
#' @return
#' A list with 3 elements:
#' 
#' eGFR_crea 
#' 
#' eGFR_cystatin 
#' 
#' eGFR_creatinine_cystatin
#' 
#' @references https://www.kidney.org/content/ckd-epi-creatinine-cystatin-equation-2021
#' 
#' https://www.kidney.org/content/ckd-epi-creatinine-equation-2021
#' 
#' https://www.kidney.org/content/ckd-epi-cystatin-c-equation-2012
#' 
#' @export
#' 
eGFR <- function(data, 
                 age_var="age", 
                 sex_var="sex", 
                 crea_var=NULL, 
                 cys_var=NULL){
  if(is.null(crea_var) & is.null(cys_var)){
    stop("At least one of crea or cys must be provided")
  }
  colnames(data)[which(colnames(data)==age_var)] <- "AGE"
  colnames(data)[which(colnames(data)==sex_var)] <- "SEX"
  data <- 
    mutate(data,
           "sex_cys"=case_when(SEX=="female"~.932,
                             SEX=="male"~1),
           "sex_crcys"=case_when(SEX=="female"~.963,
                               SEX=="male"~1),
           "sex_cr"=case_when(SEX=="female"~1.012,
                            SEX=="male"~1),
           "alpha_crcys"=case_when(SEX=="male"~-.144,
                                 SEX=="female"~-.219),
           "alpha_cr"=case_when(SEX=="male"~-.302,
                              SEX=="female"~-.241),
           "kappa_crcys"=case_when(SEX=="male"~.9,
                                 SEX=="female"~.7),
           "kappa_cr"=case_when(SEX=="male"~.9,
                              SEX=="female"~.7))
  if(!is.null(crea_var)){
    colnames(data)[which(colnames(data)==crea_var)] <- "CREA"
    data <- 
      data |> 
      rowwise() |>
      mutate(
        crea_min_cr=min(CREA/kappa_cr,1)^alpha_cr,
        crea_max_cr=max(CREA/kappa_cr,1)^-1.2,
        eGFR_cr=142*
          crea_min_cr*
          crea_max_cr*
          .9938**AGE*
          sex_cr) |>
      ungroup() 
    eGFR_crea <- data$eGFR_cr
  } else {
    eGFR_crea <- NULL
  }
  if(!is.null(cys_var)){
    colnames(data)[which(colnames(data)==cys_var)] <- "CYS"
    data <- 
      data |> 
      rowwise() |>
      mutate(
        cys_min_cys=(min(CYS/.8, 1))^-.499,
        cys_max_cys=(max(CYS/.8,1))^-1.328,
        eGFR_cys=133*
          cys_min_cys*
          cys_max_cys*
          .996**AGE*
          sex_cys
      ) |>
      ungroup()
    eGFR_cystatin <- data$eGFR_cys      
  } else {
    eGFR_cystatin <- NULL
  }
  if(!is.null(crea_var) & !is.null(cys_var)){
    data <- 
      data |> 
      rowwise() |>
      mutate(
        cys_min_crcys=(min(CYS/.8, 1))^-.323,
        cys_max_crcys=(max(CYS/.8,1))^-.778,
        crea_min_crcys=min(CREA/kappa_crcys,1)^alpha_crcys,
        crea_max_crcys=max(CREA/kappa_crcys,1)^-.544,
        eGFR_crcys=135*
          crea_min_crcys*
          crea_max_crcys*
          cys_min_cys*
          cys_max_cys*
          .9961**AGE*
          sex_crcys) |>
      ungroup()
    # eGFR_creatinine_cystatin <- data$eGFR_crcys
  } else {
    data <- 
      data |> 
      mutate(
        eGFR_creatinine_cystatin = NULL)
  }
  
  return(list(
    "eGFR_crea"=eGFR_crea, 
    "eGFR_cystatin"=eGFR_cystatin, 
    "eGFR_creatinine_cystatin"=data$eGFR_creatinine_cystatin))
}
utils::globalVariables(c("AGE", "CREA", "CYS", "alpha_cr",
                         "alpha_crcys", "crea_max_cr",
                         "crea_max_crcys",
                         "crea_min_cr", "crea_min_crcys",
                         "cys_max_cys", "cys_min_cys", "kappa_cr",
                         "kappa_crcys", "sex_cr", "sex_crcys",
                         "sex_cys"))