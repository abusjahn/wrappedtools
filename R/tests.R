#'@export
pairwise_fisher_test <- function(x,group,adjmethod='fdr',plevel=.05,
                                 symbols=c('b','c','d','e','f','g'),
                                 ref=F) {
  if (!is.factor(group))
  {group<-factor(group)}
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
    p_adj<-matrix(p.adjust(as.vector(p_unadj),method=adjmethod),byrow=F,
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
              sign_colwise=sign_colwise))
}

#'@export
pairwise_ordcat_test <- function(x,group,adjmethod='fdr',plevel=.05,
                                 symbols=letters[-1],
                                 ref=F, method='cmh') {
  x<-factor(x,ordered=T)
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
            pvalue(cmh_test(x~group,data=tempdata))
        } else {
          print('lbl_test')#(x~group,data=tempdata))
          p_unadj[secondgroup-1,firstgroup]<-
            pvalue(lbl_test(x~group,data=tempdata))
        }
      } else {
        p_unadj[secondgroup-1,firstgroup]<-1
      }
    }
  }
  sign_colwise<-character()
  if(!ref) {
    p_adj<-matrix(p.adjust(as.vector(p_unadj),method=adjmethod),byrow=F,
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

#'@export
ksnormal<-function(ksdata)
{
  ksout<-ks.test(ksdata,'pnorm',mean(ksdata,na.rm=T),sd(ksdata,na.rm=T),exact=F)
  return(ksout)
}

#'Computation and formatting of CIs for glm
#'
#'\code{glmCI} returns a list with coefficient,CIs,
#'and coef [CIs].
#'
#'@param model Output from glm .
#'@param min Smallest OR to report.
#'@param max Largest OR to report.
#'@param cisep Separator between CI values.
#'@param ndigit rounding level.
#'@export
glmCI<-function(model,min=.01,max=100, cisep='\u22ef',ndigit=2)
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
#'@export
cortestR <- function(cordata,method='pearson',
                     digits=3,digits_p=3,
                     sign='symbol'){
  n <- ncol(cordata)
  corout <- as.data.frame(
    matrix('&nbsp;',nrow = n,ncol = n))
  colnames(corout) <- colnames(cordata)
  rownames(corout) <- colnames(corout)
  for(row_i in 1:n){
    for(col_i in 1:row_i){
      ct <- cor.test(cordata[,row_i],cordata[,col_i],
                     method=method)
      corout[row_i,col_i] <-
        round(ct$estimate,digits)
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
    }
  }
  return(corout)
}

#'Two independent sample t-test with decision for var.equal based on var.test
#'
#'@param cutoff is significance threshold for equal variances
#'@export
t_var_test <- function(data,formula,cutoff=.05){
  formula <- as.formula(formula)
  t_out <- ''
  var.equal <- try(
    stats:::var.test.formula(formula=formula,
                             data=data)$p.value>cutoff,
    silent = T)
  if(is.logical(var.equal)) {
    t_out <- stats:::t.test.formula(formula=formula,data=data,
                                    var.equal = var.equal)
  }
  return(t_out)
}

#'Comparison for columns of numbers for 2 groups
#'
#'@param gaussian logical specifying normal or ordinal values.
#'@export
compare2numvars <- function(data,testvars,groupvar,
                            gaussian,round_p=3,round_desc=2,
                            range=F,pretext=F,mark=F,n=F){
  if(gaussian){
    DESC <- meansd
    COMP <- t_var_test
  } else{
    DESC <- median_quart
    COMP <- stats:::wilcox.test.formula
  }
  # descnames <- names(formals(DESC))
  # pnames <- names(formals(COMP))

  data_l <- data %>%
    dplyr::select(Group=groupvar, testvars) %>%
    mutate(Group=factor(Group)) %>%
    gather(key = Variable,value = Value,-Group) %>%
    # na.omit() %>%
    as.tibble()
  out <- data_l %>%
    group_by(Variable) %>%
    do(summarise(.data = .,
                 n_groups=paste(table(.$Group[which(!is.na(.$Value))]),collapse=':'),
                 desc_all=DESC(.$Value,
                               roundDig = round_desc,range=range),
                 desc_groups=paste(try(
                   DESC(x = .$Value,groupvar = .$Group,
                        roundDig = round_desc, range=range),
                   silent = T),
                   collapse = ':'),
                 p = formatP(try(
                   COMP(data=.,formula = Value~Group)$p.value,
                   silent = T),
                   ndigits = round_p,pretext = pretext, mark=mark)))
    out$desc_groups[!str_detect(out$desc_groups,':')] <- ' : '
    out <- separate(out,col = desc_groups,
             into = glue::glue('{groupvar} {levels(data_l$Group)}'),
             sep = ':')
    out <- separate(out,col = n_groups,
                    into = glue::glue('n {groupvar} {levels(data_l$Group)}'),
                    sep = ':')
    if(n==F){
      out <- select(out,-starts_with('n'))
    }
    return(out)
}
#'Comparison for columns of factors for 2 groups
#'
#'@export
compare2qualvars <- function(data,testvars,groupvar,
                             round_p=3,round_desc=2,
                             pretext=F,mark=F,
                             singleline=F,
                             newline=T){
  # data[,groupvar] <- factor(data[,groupvar])
  freq <-
    map(data[testvars],
        .f = function(x) cat_desc_stats(
          x,return_level = F,singleline=singleline)) %>%
    map(as.tibble)


  levels <-
    map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        singleline=singleline)$level) %>%
    map(as.tibble)
  # freqBYgroup <- apply(data[testvars],2,
  #               FUN = function(x) by(x,data[groupvar],cat_desc_stats,
  #                                    return_level = F)) %>% t()
  freqBYgroup <-
    map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        groupvar=data[[groupvar]],
                                        return_level = F,
                                        singleline=singleline))

  # map(data[testvars],
  #                  .f = function(x) cat_desc_stats(x,return_level = F))# %>%
  # transpose() %>% as.tibble()
  p <-
    map2(data[testvars],data[groupvar],
         .f = function(x,y) formatP(try(
           fisher.test(x = x,y = y)$p.value,silent=T),
           mark = mark,pretext = pretext))

  # colnames(freqBYgroup) <- glue::glue('{groupvar} {factor(levels(data[[groupvar]]))}')
  out <- tibble(Variable=character(),desc_all=character(),
                g1=character(),g2=character(),p=character())
  for(var_i in seq_along(testvars)){
    if(newline){
      out <- add_row(out,Variable=c(testvars[var_i],
                                    glue::glue(
                                      '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{levels[[var_i]][[1]]}')),
                     desc_all=c('',freq[[var_i]][[1]]),
                     g1=c('',freqBYgroup[[var_i]][[1]]),
                     g2=c('',freqBYgroup[[var_i]][[2]]),
                     p=c(p[[var_i]][[1]],
                         rep('  ',nrow(freqBYgroup[[var_i]]))))

    } else{
      out <- add_row(out,Variable=paste(c(testvars[var_i],
                                          rep('&nbsp;&nbsp;&nbsp;&nbsp;',
                                              nrow(freqBYgroup[[var_i]])-1)),
                                        levels[[var_i]][[1]]),
                     desc_all=freq[[var_i]][[1]],
                     g1=freqBYgroup[[var_i]][[1]],
                     g2=freqBYgroup[[var_i]][[2]],
                     p=c(p[[var_i]][[1]],
                         rep('  ',nrow(freqBYgroup[[var_i]])-1)))
    }
  }
  return(out)
}


#'Comparison for columns of factors for 2 groups with furrr
#'
#'@export
compare2qualvars_f <- function(data,testvars,groupvar,
                             round_p=3,round_desc=2,
                             pretext=F,mark=F,
                             singleline=F,
                             newline=T){
  # data[,groupvar] <- factor(data[,groupvar])
  freq <-
    future_map(data[testvars],
        .f = function(x) cat_desc_stats(
          x,return_level = F,singleline=singleline)) %>%
    future_map(as.tibble)


  levels <-
    future_map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        singleline=singleline)$level) %>%
    map(as.tibble)
  # freqBYgroup <- apply(data[testvars],2,
  #               FUN = function(x) by(x,data[groupvar],cat_desc_stats,
  #                                    return_level = F)) %>% t()
  freqBYgroup <-
    future_map(data[testvars],
        .f = function(x) cat_desc_stats(x,
                                        groupvar=data[[groupvar]],
                                        return_level = F,
                                        singleline=singleline))

  # map(data[testvars],
  #                  .f = function(x) cat_desc_stats(x,return_level = F))# %>%
  # transpose() %>% as.tibble()
  p <-
    future_map2(data[testvars],data[groupvar],
         .f = function(x,y) formatP(try(
           fisher.test(x = x,y = y)$p.value,silent=T),
           mark = mark,pretext = pretext))

  # colnames(freqBYgroup) <- glue::glue('{groupvar} {factor(levels(data[[groupvar]]))}')
  out <- tibble(Variable=character(),desc_all=character(),
                g1=character(),g2=character(),p=character())
  for(var_i in seq_along(testvars)){
    if(newline){
      out <- add_row(out,Variable=c(testvars[var_i],
                                    glue::glue(
                                      '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{levels[[var_i]][[1]]}')),
                     desc_all=c('',freq[[var_i]][[1]]),
                     g1=c('',freqBYgroup[[var_i]][[1]]),
                     g2=c('',freqBYgroup[[var_i]][[2]]),
                     p=c(p[[var_i]][[1]],
                         rep('  ',nrow(freqBYgroup[[var_i]]))))

    } else{
      out <- add_row(out,Variable=paste(c(testvars[var_i],
                                          rep('&nbsp;&nbsp;&nbsp;&nbsp;',
                                              nrow(freqBYgroup[[var_i]])-1)),
                                        levels[[var_i]][[1]]),
                     desc_all=freq[[var_i]][[1]],
                     g1=freqBYgroup[[var_i]][[1]],
                     g2=freqBYgroup[[var_i]][[2]],
                     p=c(p[[var_i]][[1]],
                         rep('  ',nrow(freqBYgroup[[var_i]])-1)))
    }
  }
  return(out)
}

