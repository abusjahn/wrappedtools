#'Calculate pairwise comparisons between group levels with corrections for 
#'multiple testing
#'
#'@param x dependent variable, containing the data
#'@param group independent variable, should be factor
#'@param adjmethod method for adjusting p values (see [p.adjust])
#'@param plevel threshold for significance
#'@param symbols predefined as b,c, d...;  provides footnotes to mark group 
#'differences, e.g. b means different from group 2
#'@param ref is the 1st subgroup the reference (like in Dunnett test)
#'export
pairwise_fisher_test <- function(x,group,adjmethod='fdr',plevel=.05,
                                 symbols=letters[-1],#c('b','c','d','e','f','g'),
                                 ref=FALSE) {
  if (!is.factor(group)) {
    group<-factor(group)}
  ngroups<-length(levels(group))
  pft_data<-data.frame(x,group)
  pft_data<-na.omit(pft_data)
  #print(pft_data)
  p_unadj<-matrix(nrow=ngroups-1,ncol=ngroups-1,
                  dimnames=list(c(2:ngroups),c(1:(ngroups-1))))
  for (firstgroup in 1:(ngroups-1)) {
    for (secondgroup in (firstgroup+1):ngroups) {
      tempdata<-pft_data[which(
        pft_data$group==levels(as.factor(pft_data$group))[firstgroup]|
          pft_data$group==levels(as.factor(pft_data$group))[secondgroup]),]
      if (min(dim(table(tempdata)))>1) {
        p_unadj[secondgroup-1,firstgroup]<-
          fisher.test(tempdata$x,tempdata$group)$p.value
      } else {
        p_unadj[secondgroup-1,firstgroup]<-1
      }
    }
  }
  #print(p_unadj)
  sign_colwise<-character()
  if(!ref) {
    p_adj<-matrix(p.adjust(as.vector(p_unadj),method=adjmethod),byrow=FALSE,
                  nrow=ngroups-1,ncol=ngroups-1,
                  dimnames=list(c(2:ngroups),c(1:(ngroups-1))))
    for (col_i in 1:ncol(p_adj)) {
      temp<-' '
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i,col_i]) & p_adj[row_i,col_i]<plevel) {
          temp<-paste0(temp,symbols[row_i])
        }
      }
      sign_colwise<-c(sign_colwise,temp)
    }
  } else {
    p_adj<-p.adjust(as.vector(p_unadj[,1]),method=adjmethod)
    sign_colwise<-markSign(p_adj)#sapply(p_adj,markSign)
  }
  return(list(method=adjmethod,
              p.value=p_adj,
              plevel=plevel,
              sign_colwise=sign_colwise))
}

#'Calculate pairwise comparisons for ordinal categories between group levels 
#'with corrections for multiple testing
#'
#'@param x dependent variable, containing the data
#'@param group independent variable, should be factor
#'@param adjmethod method for adjusting p values (see [p.adjust])
#'@param plevel threshold for significance
#'@param symbols predefined as b,c, d...;  provides footnotes to mark group 
#'differences, e.g. b means different from group 2
#'@param ref is the 1st subgroup the reference (like in Dunnett test)
#'@param method which function should be used for individual testing?
#'Predefined to cmh from package coin
#'export
#'@export
pairwise_ordcat_test <- function(x,group,adjmethod='fdr',plevel=.05,
                                 symbols=letters[-1],
                                 ref=FALSE, method='cmh') {
  x<-factor(x,ordered=TRUE)
  group<-factor(group)
  ngroups<-length(levels(group))
  pft_data<-data.frame(x,group)
  pft_data<-na.omit(pft_data)
  p_unadj<-matrix(nrow=ngroups-1,ncol=ngroups-1,
                  dimnames=list(c(2:ngroups),c(1:(ngroups-1))))
  for (firstgroup in 1:(ngroups-1)) {
    for (secondgroup in (firstgroup+1):ngroups) {
      tempdata<-pft_data[which(
        as.numeric(pft_data$group) %in% c(firstgroup,secondgroup)),]
      if (min(dim(table(tempdata)))>1) {
        if(method=='cmh') {
          print('cmh_test')#(x~group,data=tempdata))
          p_unadj[secondgroup-1,firstgroup]<-
            coin::pvalue(coin::cmh_test(x~group,data=tempdata))
        } else {
          print('lbl_test')#(x~group,data=tempdata))
          p_unadj[secondgroup-1,firstgroup]<-
            coin::pvalue(coin::lbl_test(x~group,data=tempdata))
        }
      } else {
        p_unadj[secondgroup-1,firstgroup]<-1
      }
    }
  }
  sign_colwise<-character()
  if(!ref) {
    p_adj<-matrix(p.adjust(as.vector(p_unadj),method=adjmethod),byrow=FALSE,
                  nrow=ngroups-1,ncol=ngroups-1,
                  dimnames=list(c(2:ngroups),c(1:(ngroups-1))))
    for (col_i in 1:ncol(p_adj)) {
      temp<-' '
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i,col_i]) & p_adj[row_i,col_i]<plevel) {
          temp<-paste0(temp,symbols[row_i])
        }
      }
      sign_colwise<-c(sign_colwise,temp)
    }
  } else {
    p_adj<-p.adjust(as.vector(p_unadj[,1]),method=adjmethod)
    sign_colwise<-sapply(p_adj,markSign)
  }
  return(list(method=adjmethod,
              p.value=p_adj,
              plevel=plevel,
              sign_colwise=sign_colwise,
              method=method))
}

#'Convinience function around ks.test, testing against Normal distribution
#'
#'\code{ksnormal} returns p.value from ks.test.
#'
#'@param ksdata Vector of data to test.
#'@export
ksnormal<-function(ksdata)
{
  options(warn=-1)
  ksout<-ks.test(ksdata,'pnorm',mean(ksdata,na.rm=TRUE),sd(ksdata,na.rm=TRUE),
                 exact=FALSE)$p.value
  options(warn=0)
  return(ksout)
}

#'Computation and formatting of CIs for glm
#'
#'\code{glmCI} returns a list with coefficient, CIs, and coef \[CIs\].
#'
#'@param model Output from glm .
#'@param min Smallest OR to report.
#'@param max Largest OR to report.
#'@param cisep Separator between CI values.
#'@param ndigit rounding level.
#'
#'@usage glmCI(model, min = .01, max = 100, cisep = '\U000022ef', ndigit=2)
#'
#'@export
glmCI<-function(model, min = .01, max = 100, cisep = '\U000022ef', ndigit=2)
{
  glmReturn<-list(coeff=character(0),ci=character(0),
                  c_ci=NA)
  pvTmp<-labels(model$terms)
  for (pv_i in 1:length(pvTmp))
  {
    rows<-grep(pvTmp[pv_i],names(model$coefficients))
    coeffTmp<-as.character(as.vector(round(exp(model$coefficients[rows]),ndigit)))
    coeffTmp[which(as.numeric(coeffTmp)<min)]<-paste0('<',min)
    coeffTmp[which(as.numeric(coeffTmp)>max)]<-paste0('>',max)
    glmReturn$coeff<-c(glmReturn$coeff,paste(coeffTmp,collapse='/'))
    ciModel<-as.matrix(exp(confint(model)))
    ciTmp<-character(0)
    for (row_i in rows)
    {
      ciRow<-as.character(as.vector(round(ciModel[row_i,],ndigit)))
      ciRow[which(as.numeric(ciRow)<min)]<-paste0('<',min)
      ciRow[which(as.numeric(ciRow)>max)]<-paste0('>',max)
      ciTmp<-paste(ciTmp,paste(ciRow,collapse=cisep),
                   sep='/')
    }
    glmReturn$ci<-c(glmReturn$ci,gsub('^/','',ciTmp))
  }
  glmReturn$c_ci<-paste0(glmReturn$coeff,' (',glmReturn$ci,')')
  return(glmReturn)
}


#'correlation matrix with p-values based on cor.test
#'
#'\code{cortestR} returns a data frame with coefficient,p-values,
#'and significance symbols.
#'
#'@param cordata data frame or matrix with rawdata.
#'@param method as in cor.test.
#'@param digits rounding level for estimate.
#'@param digits_p rounding level for p value.
#'@param sign if 'symbol', add significance indicator.
#'@param split logical, report correlation and p combined (default) or split in list
#'@export
cortestR <- function(cordata,method='pearson',
                     digits=3,digits_p=3,
                     sign='symbol',
                     split=FALSE){
  n <- ncol(cordata)
  corout <- as.data.frame(
    matrix('&nbsp;',nrow = n,ncol = n))
  colnames(corout) <- colnames(cordata)
  rownames(corout) <- colnames(corout)
  if(split){
    pout <- corout
  }
  for(row_i in 1:n){
    for(col_i in 1:row_i){
      ct <- cor.test(cordata[,row_i],cordata[,col_i],
                     method=method)
      corout[row_i,col_i] <-
        round(ct$estimate,digits)
        if(!split){
          if(row_i!=col_i){
            if(sign=='symbol'){
            corout[row_i,col_i] <- paste0( corout[row_i,col_i],
                                           markSign(ct$p.value))
          } else {
            corout[row_i,col_i] <- paste0( corout[row_i,col_i], ' (',
                                           formatP(ct$p.value,ndigits = digits_p),
                                           ')')
          }
        } 
        } else{
        pout[row_i,col_i] <- ct$p.value
      }
    }
  }
  if(split){
    return(list(corout=corout,
                pout=pout))
    } else {
      return(corout)
    }
}

#'Two independent sample t-test with decision for var.equal based on var.test
#'
#'@param data Tibble or data_frame.
#'@param formula Formula object with dependent and independent variable.
#'@param cutoff is significance threshold for equal variances
#'@export
t_var_test <- function(data,formula,cutoff=.05){
  formula <- as.formula(formula)
  t_out <- ''
  var.equal <- try(
    var.test(formula=formula,
                             data=data)$p.value>cutoff,
    silent = TRUE)
  if(is.logical(var.equal)) {
    t_out <- t.test(formula=formula,data=data,
                                    var.equal = var.equal)
  }
  return(t_out)
}

#'Comparison for columns of numbers for 2 groups
#'
#'@param data name of dataset (tibble/data.frame) to analyze.
#'@param testvars vector of column names.
#'@param groupvar name of grouping variable, has to translate to 2 groups.
#'@param gaussian logical specifying normal or ordinal values.
#'@param round_p level for rounding p-value
#'@param round_desc number of significant digits for rounding of descriptive stats
#'@param range include min/max?
#'@param rangesep text between statistics and range or other elements  
#'@param pretext for function [formatP]
#'@param mark for function [formatP]
#'@param n create columns for n per group?
#'@param .n add n to descriptive statistics?
#'@export
compare2numvars <- function(data,testvars,groupvar,
                            gaussian,round_p=3,round_desc=2,
                            range=FALSE,
                            rangesep=' ',
                            pretext=FALSE,mark=FALSE,n=FALSE,.n=FALSE){
  `.` <- Group <- Value <- Variable <- desc_groups <- NULL
    options(warn=-1)
  if(gaussian){
    DESC <- meansd
    COMP <- t_var_test
  } else{
    DESC <- median_quart
    COMP <- wilcox.test
  }
  # descnames <- names(formals(DESC))
  # pnames <- names(formals(COMP))

  data_l <- data %>%
    dplyr::select(Group=all_of(groupvar), 
                  all_of(testvars)) %>%
    mutate(Group=factor(Group)) %>%
    gather(key = Variable,value = Value,-Group) %>%
    mutate(Variable=forcats::fct_inorder(Variable)) %>%
    # na.omit() %>%
    as_tibble()
  out <- data_l %>%
    group_by(Variable) %>%
    do(summarise(.data = .,
                 n_groups=paste(table(.$Group[which(!is.na(.$Value))]),collapse=':'),
                 desc_all=DESC(.$Value,
                               roundDig = round_desc,
                               range=range,rangesep=rangesep,
                               .n=.n),
                 desc_groups=paste(try(
                   DESC(x = .$Value,groupvar = .$Group,
                        roundDig = round_desc, range=range,
                        rangesep=rangesep, .n=.n),
                   silent = TRUE),
                   collapse = ':'),
                 p = formatP(try(
                   COMP(formula = as.formula("Value~Group"),data=.)$p.value,
                   silent = TRUE),
                   ndigits = round_p,pretext = pretext, 
                   mark=mark) %>% as.character()))
    out$desc_groups[!str_detect(out$desc_groups,':')] <- ' : '
    out <- separate(out,col = desc_groups,
             into = glue::glue('{groupvar} {levels(data_l$Group)}'),
             sep = ':')
    out <- separate(out,col = n_groups,
                    into = glue::glue('n {groupvar} {levels(data_l$Group)}'),
                    sep = ':')
    out$n <- apply(out[,2:3],1,function(x){sum(as.numeric(x))})
    out %<>% dplyr::select(1,n,starts_with('n '),everything())

    if(n==FALSE){
      out <- dplyr::select(out,-n,-starts_with('n '))
    }
    options(warn=0)
    return(out)
}
#'Comparison for columns of factors for 2 groups
#'@param data name of data set (tibble/data.frame) to analyze.
#'@param testvars vector of column names.
#'@param groupvar name of grouping variable, has to translate to 2 groups.
#'@param round_p level for rounding p-value.
#'@param round_desc number of significant digits for rounding of descriptive stats
#'@param pretext for function [formatP]
#'@param mark for function [formatP]
#'@param singleline Put all group levels in  a single line?
#'@param spacer Text element to indent levels.  
#'@param linebreak place holder for newline.
#'
#'@export
compare2qualvars <- function(data,testvars,groupvar,
                             round_p=3,round_desc=2,
                             pretext=FALSE,mark=FALSE,
                             singleline=FALSE,
                             # newline=TRUE,
                             spacer='&nbsp;',
                             linebreak='\n'){
  if(!(is.factor(data %>% pull(groupvar)))) {
    data %<>% mutate(!!groupvar:=factor(!!sym(groupvar)))
  }
  freq <-
    purrr::map(data[testvars],
        .f = function(x) cat_desc_stats(
          x,return_level = FALSE,singleline=singleline,
          ndigit=round_desc)) %>%
    purrr::map(as_tibble)


  levels <-
    purrr::map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        singleline=singleline)$level) %>%
    purrr::map(as_tibble)
  # freqBYgroup <- apply(data[testvars],2,
  #               FUN = function(x) by(x,data[groupvar],cat_desc_stats,
  #                                    return_level = FALSE)) %>% t()
  freqBYgroup <-
    purrr::map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        groupvar=data[[groupvar]],
                                        return_level = FALSE,
                                        ndigit=round_desc,
                                        singleline=singleline))

  # purrr::map(data[testvars],
  #                  .f = function(x) cat_desc_stats(x,return_level = FALSE))# %>%
  # transpose() %>% as_tibble()
  p <-
    purrr::map2(data[testvars],data[groupvar],
         .f = function(x,y) formatP(try(
           fisher.test(x = x,y = y,simulate.p.value = TRUE,B = 10^4)$p.value,silent=TRUE),
           mark = mark,pretext = pretext))

  # colnames(freqBYgroup) <- glue::glue('{groupvar} {factor(levels(data[[groupvar]]))}')
  out <- tibble(Variable=character(),desc_all=character(),
                g1=character(),g2=character(),p=character())
  for(var_i in seq_along(testvars)){
    if(!singleline){
      out <- add_row(out,Variable=c(testvars[var_i],
                                    glue::glue(
                                      '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{levels[[var_i]][[1]]}')),
                     desc_all=c(spacer,freq[[var_i]][[1]]),
                     g1=c(spacer,freqBYgroup[[var_i]][[1]]),
                     g2=c(spacer,freqBYgroup[[var_i]][[2]]),
                     p=c(p[[var_i]][[1]],
                         rep(spacer,nrow(freqBYgroup[[var_i]]))))

    } else{
      out <- add_row(out,Variable=paste(c(testvars[var_i],
                                          rep('&nbsp;&nbsp;&nbsp;&nbsp;',
                                              nrow(freqBYgroup[[var_i]])-1)),
                                        levels[[var_i]][[1]]),
                     desc_all=freq[[var_i]][[1]],
                     g1=freqBYgroup[[var_i]][[1]],
                     g2=freqBYgroup[[var_i]][[2]],
                     p=p[[var_i]][[1]])
    }
  }
  colnames(out) %<>% str_replace_all(
    c('g1'=paste(groupvar,
                 data %>% pull(groupvar) %>% levels() %>% first()),
      'g2'=paste(groupvar,
                 data %>% pull(groupvar) %>% levels() %>% last())
    )
  )
  return(out)
}

#'Comparison for columns of factors for more than 2 groups with post-hoc
#'@param data name of data set (tibble/data.frame) to analyze.
#'@param testvars vector of column names.
#'@param groupvar name of grouping variable, has to translate to 2 groups.
#'@param round_p level for rounding p-value.
#'@param round_desc number of significant digits for rounding of descriptive stats
#'@param pretext for function [formatP]
#'@param mark for function [formatP]
#'@param singleline Put all group levels in  a single line?
#'@param spacer Text element to indent levels.  
#'@param linebreak place holder for newline.
#'@param prettynum Apply prettyNum to results?
#'
#'@export
compare_n_qualvars <- function(data,testvars,groupvar,
                             round_p=3,round_desc=2,
                             pretext=FALSE,mark=FALSE,
                             singleline=FALSE,
                             # newline=TRUE,
                             spacer='&nbsp;',
                             linebreak='\n',
                             prettynum=FALSE){
  if(!(is.factor(data %>% pull(groupvar)))) {
    data %<>% mutate(!!groupvar:=factor(!!sym(groupvar)))
  }
  # groups <- levels(data[[groupvar]])
   freq <-
    purrr::map(data[testvars],
               .f = function(x) cat_desc_stats(
                 x,return_level = FALSE,singleline=singleline,
                 ndigit=round_desc,separator = linebreak,
                 prettynum = prettynum)) %>%
    purrr::map(as_tibble)
  
  
  levels <-
    purrr::map(data[testvars],
               .f = function(x) cat_desc_stats(x,
                                               singleline=singleline,
                                               separator = linebreak)$level) %>%
    purrr::map(as_tibble)
  freqBYgroup <-
    purrr::map(data[testvars],
               .f = function(x) cat_desc_stats(x,
                                               groupvar=data[[groupvar]],
                                               return_level = FALSE,
                                               ndigit=round_desc,
                                               singleline=singleline,
                                               separator = linebreak,
                                               prettynum = prettynum))
  
  p <-
    purrr::map2(data[testvars],data[groupvar],
                .f = function(x,y) formatP(try(
                  fisher.test(x = x,y = y,simulate.p.value = TRUE,
                              B = 10^4)$p.value,silent=TRUE),
                  mark = mark,pretext = pretext))
  
  out <- tibble(Variable=character(),desc_all=character()) %>% 
    left_join(freqBYgroup[[1]] %>% slice(0),by = character()) %>% 
    mutate(p=character())
  out_template <- out
  groupcols <- 3:(ncol(out)-1)
  for(var_i in seq_along(testvars)){
    testdata <- data %>% dplyr::select(all_of(c(groupvar,testvars[var_i]))) %>% 
      na.omit()
    pairwise_p <- pairwise_fisher_test(testdata[[2]],testdata[[1]])$sign_colwise %>% 
      str_replace('^ $',spacer)
    if(!singleline){
      out_tmp <- add_row(out_template,
                         Variable=c(
                           testvars[var_i],
                           str_glue(
                             '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{levels[[var_i]][[1]]}')),
                         desc_all=c(spacer,freq[[var_i]][[1]]))
      out_tmp[1,groupcols] <- c(pairwise_p, spacer) %>% as.list()
      out_tmp[-1,groupcols] <-freqBYgroup[[var_i]]
      out_tmp['p'] <- c(p[[var_i]][[1]],
                         rep(spacer,nrow(freqBYgroup[[var_i]])))
      
    } else{
      out_tmp <- add_row(out_template,Variable=paste(c(testvars[var_i],
                                          rep('&nbsp;&nbsp;&nbsp;&nbsp;',
                                              nrow(freqBYgroup[[var_i]])-1)),
                                        levels[[var_i]][[1]]),
                     desc_all=freq[[var_i]][[1]])
      out_tmp[1,groupcols] <- paste(freqBYgroup[[var_i]],c(pairwise_p,spacer)) %>% 
        as.list()
      out_tmp['p'] <- p[[var_i]]
    }
  out %<>% rbind(out_tmp)  
  }
  return(out)
}


#'Calculate pairwise comparisons between group levels with corrections for 
#'multiple testing
#'
#'@param dep_var dependent variable, containing the data
#'@param indep_var independent variable, should be factor
#'@param strat_var optiona factor for stratification
#'@param adjmethod method for adjusting p values (see [p.adjust])
#'@param distr The conditional null distribution of the test statistic is used to obtain p-values and an asymptotic approximation of the exact distribution is used by default (distribution = "asymptotic"). Alternatively, the distribution can be approximated via Monte Carlo resampling or computed exactly for univariate two-sample problems by setting distribution to "approximate" or "exact" respectively.
#'@param plevel threshold for significance
#'@param symbols predefined as b,c, d...;  provides footnotes to mark group 
#'differences, e.g. b means different from group 2
#'@param sep text between statistics and range or other elements  
#'@export
pairwise_wilcox_test <-
  function(dep_var,indep_var,strat_var=NA,
           adjmethod='fdr',distr='exact',plevel=.05,
           symbols=letters[-1],
           sep='')
  {
    # if (!is.factor(indep_var))
    # {
    indep_var<-factor(indep_var)
    # }
    ngroups<-length(levels(indep_var))
    if (length(strat_var)==1) {
      strat_var<-rep(1,length(dep_var))
      }
    if (!is.factor(strat_var)) {
      strat_var<-factor(strat_var)
      }
    pwt_data<-tibble(dep_var,
                         indep_var=as.numeric(indep_var),
                         strat_var)
    p_unadj<-matrix(nrow=ngroups-1,ncol=ngroups-1,
                    dimnames=list(c(2:ngroups),c(1:(ngroups-1))))
    for (firstgroup in 1:(ngroups-1)) {
      for (secondgroup in (firstgroup+1):ngroups) {
        tempdata<-pwt_data[c(which(pwt_data$indep_var==firstgroup),
                             which(pwt_data$indep_var==secondgroup)),]
        tempdata$indep_var<-factor(tempdata$indep_var)
        #print(tempdata)
        if (length(levels(as.factor(tempdata$dep_var)))>1) {
          p_unadj[secondgroup-1,firstgroup]<-
            coin::pvalue(coin::wilcox_test(tempdata$dep_var~tempdata$indep_var |
                                 tempdata$strat_var,
                               distribution=distr))
        }
        else {
          p_unadj[secondgroup-1,firstgroup]<-1
        }
      }
    }
    p_adj<-matrix(p.adjust(as.vector(p_unadj),method=adjmethod),byrow=FALSE,
                  nrow=ngroups-1,ncol=ngroups-1,
                  dimnames=list(group2=levels(indep_var)[-1],
                                group1=levels(indep_var)[-ngroups]))
    sign_colwise<-character()
    for (col_i in 1:ncol(p_adj)) {
      temp<-' '
      for (row_i in col_i:nrow(p_adj)) {
        if (!is.na(p_adj[row_i,col_i]) &
            p_adj[row_i,col_i]<plevel) {
          temp<-paste(temp,symbols[row_i],sep=sep)
        }
      }
      sign_colwise<-c(sign_colwise,temp)
    }

    return(list(p_adj=p_adj,
                sign_colwise=sign_colwise))
  }

#'Calculate pairwise comparisons between group levels with corrections for 
#'multiple testing, extension of pairwise.t.test
#'
#'@param dep_var dependent variable, containing the data
#'@param indep_var independent variable, should be factor
#'@param adjmethod method for adjusting p values (see [p.adjust])
#'@param plevel threshold for significance
#'@param symbols predefined as b,c, d...;  provides footnotes to mark group 
#'differences, e.g. b means different from group 2
#'@export
pairwise_t_test<-function(dep_var,indep_var,adjmethod='fdr',plevel=.05,
                          symbols=letters[-1])
{
  t_out<-pairwise.t.test(x=dep_var,g=indep_var,p.adjust.method=adjmethod)
  p_colwise<-t_out$p.value
  sign_colwise<-character()
  for (col_i in 1:ncol(p_colwise)) {
    temp<-' '
    for (row_i in col_i:nrow(p_colwise)) {
      if (!is.na(p_colwise[row_i,col_i]) &
          p_colwise[row_i,col_i]<plevel) {
        temp<-paste0(temp,symbols[row_i])
      }
    }
    sign_colwise<-c(sign_colwise,temp)
  }
  return(list(method=t_out$method,
              p.value=t_out$p.value,
              plevel=plevel,
              sign_colwise=sign_colwise))

}


#'Comparison for columns of Gaussian measures for n groups
#'
#'@param .data name of dataset (tibble/data.frame)to analyze, defaults to rawdata.
#'@param testvars vector of column names.
#'@param groupvar name of grouping variable.
#'@param round_p level for rounding p-value
#'@param round_desc number of significant digits for rounding of descriptive stats
#'@param range include min/max?
#'@param rangesep text between statistics and range or other elements  
#'@param pretext for function formatP
#'@param mark for function formatP
#'@param .n add n to descriptive statistics?
#'@export
compare_n_numvars <- function(.data=rawdata,
                              testvars,groupvar,# gaussian,
                              round_desc=2,range=FALSE,
                              rangesep=' ',
                              pretext=FALSE,mark=FALSE,round_p=3,
                              .n=FALSE) {
value <- Variable <- lmout <- p_tout <- pANOVA <- NULL
    # if(gaussian){
  #   desc <- wrappedtools::meansd
  #   # grptest <- lm
  # } else {
  #   desc <- wrappedtools::median_quart
  #   # grptest <- kruskal.test
  # }
  if(!is.factor(.data[[groupvar]]) |
     is.ordered(.data[[groupvar]])){
    .data[[groupvar]] <- factor(.data[[groupvar]],
                                ordered = FALSE)
  }
  glevel <- forcats::fct_inorder(levels(.data[[groupvar]]))
  .data <- dplyr::select(.data,all_of(testvars),
                         all_of(groupvar))
  t <- .data %>% gather(key = 'Variable',value = 'value',
                        all_of(testvars)) %>%
    nest(data=c(groupvar,value)) %>%
    mutate(Variable=forcats::fct_inorder(Variable),
           desc=purrr::map_chr(data,~meansd(.$value,
                                     roundDig = round_desc,
                                     range = range,
                                     rangesep=rangesep,
                                     .n = .n)),
           desc_grp=purrr::map(data,~meansd(.$value,
                                   groupvar = .[groupvar],
                                   roundDig = round_desc,
                                   range = range,
                                   rangesep=rangesep,
                                   .n = .n)) %>%
             purrr::map(~set_names(.x,
                            as.character(glevel))) ,
           lmout=purrr::map(data,~lm(value~!!sym(groupvar),data=.x)),
           aout=purrr::map(lmout,anova),
           ptout=purrr::map(data,~pairwise.t.test(.x[['value']],
                                           g=.x[[groupvar]],
                                           pool.sd = TRUE,
                                           p.adjust.method = 'none')$p.value),
           p_tout=purrr::map(data,~pairwise_t_test(.x[['value']],
                                            .x[[groupvar]])$sign_colwise),
           p_tout=purrr::map(p_tout,~c(.x,''))) %>%
    purrr::map(~set_names(.x,testvars))

  results <- tibble(Variable=forcats::fct_inorder(testvars),all=t$desc) %>%
    full_join(purrr::reduce(t$desc_grp,rbind) %>%
                matrix(nrow=length(testvars),byrow=FALSE) %>%
                as_tibble(.name_repair='unique') %>%
                mutate(Variable=testvars) %>%
                dplyr::select(Variable, everything()) %>%
                set_names(c('Variable',
                            paste(groupvar,glevel)))) %>%
    full_join(purrr::map_df(t$aout, 'Pr(>F)') %>% slice(1) %>%
                gather(key='Variable',value = 'pANOVA') %>%
                mutate(Variable=forcats::fct_inorder(Variable),
                       pANOVA=formatP(pANOVA,
                                      ndigits=round_p,
                                      pretext=pretext,
                                      mark=mark) %>% as.vector())) %>%
    full_join(purrr::map_df(t$ptout,~paste(formatP(
      p.adjust(.x[lower.tri(.x,TRUE)], method='fdr')),
                                    collapse = ';')) %>%
                gather(key='Variable',value = 'p between groups' )) %>%
    full_join(purrr::reduce(t$p_tout,rbind) %>%
                matrix(nrow=length(testvars),byrow=FALSE) %>%
                as_tibble() %>%
                         mutate(Variable=testvars) %>%
                set_names(c(paste('sign',glevel),'Variable'))) %>%
    full_join(purrr::map_df(t$ptout,~paste(formatP(p.adjust(.x[,1],
                                                            method='fdr')),
                                    collapse = ';')) %>%
                gather(key='Variable',value = 'p vs.ref' ))
  results <- cbind(results,
                   purrr::map2_df(.x = dplyr::select(results,starts_with(groupvar)),
                           .y = dplyr::select(results, starts_with('sign')),
                           .f = paste) %>%
                     rename_all(paste,'fn')) %>%
    as_tibble()
  #todo: p vs. ref symbol

    return(list(results=results,raw=t))
}
# cnn <- compare_n_numvars(.data = diamonds,
#                          testvars = c('carat','x','y','z'),
#                          groupvar = 'cut')
