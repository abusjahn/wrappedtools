#'Compute mean and sd and put together with the +- symbol.
#'
#'@param x Data for computation.
#'@param roundDig Number of relevant digits for roundR.
#'@param drop0 Should trailing zeros be dropped?
#'@param groupvar Optional grouping variable for subgroups.
#'@param range Should min and max be included in output?
#'@param rangesep How should min/max be separated from mean+-sd?
#'@param .n Should n be included in output?
#'Default ARRR is my shortcut for newline ^p later in Word.
#'@param .n Should n be included in output?
#'@param .german logical, should "." and "," be used as bigmark and decimal?
#'@examples
#' # basic usage of meansd
#' \dontrun{
#' meansd(x=mtcars$wt)
#' }
#' @export
meansd<-function(x,roundDig=2,drop0=F,groupvar=NULL,
                 range=F,rangesep='ARRR',.n=F, .german=F) {
  out<-''
  if(length(na.omit(x))>0) {
    if(is.null(groupvar)) {
      meansd<-cbind(roundR(
        matrix(c(mean(x,na.rm = T),
                 sd(x,na.rm = T),
                 min(x,na.rm = T),
                 max(x,na.rm = T)),
               ncol=4,byrow = F),level=roundDig,
        drop0=drop0,.german=.german),
        length(na.omit(x)))
    } else {
      meansd<- matrix(c(by(x,groupvar,mean,na.rm=T),
                        by(x,groupvar,sd,na.rm=T),
                        by(x,groupvar,min,na.rm=T),
                        by(x,groupvar,max,na.rm=T)),
                      ncol=4,byrow=F)%>% na_if(Inf) %>% na_if(-Inf) %>%
      roundR(level=roundDig, drop0=drop0,.german=.german) %>%
        cbind(by(x,groupvar,function(x){length(na.omit(x))}))
    }
    out<-paste(meansd[,1],meansd[,2],sep='\u00B1')
    if(range) {
      out<-paste0(out,rangesep, ' [',
                  apply(matrix(meansd[,3:4],ncol=2),1,paste,
                        collapse=' -> '),']') #\u22ef
    }
    if(.n) {
      out<-paste0(out,rangesep, ' [n=',
                  meansd[,5],']') #\u22ef
    }
  }#   }
  return(out)
}

#'Compute median and quartiles and put together.
#'
#'@param x Data for computation.
#'@param nround Number of digits for fixed round.
#'@param probs Quantiles to compute.
#'@param qtype Type of quantiles.
#'@param roundDig Number of relevant digits for roundR.
#'@param drop0 Should trailing zeros be dropped?
#'@param groupvar Optional grouping variable for subgroups.
#'@param range Should min and max be included in output?
#'@param rangesep How should min/max be separated from mean+-sd?
#'@param rangearrow What is put between min -> max?
#'Default ARRR is my shortcut for newline ^p later in Word.
#'#'@param prettynum logical, apply prettyNum to results?
#'@param .german logical, should "." and "," be used as bigmark and decimal?
#'@param n Should n be included in output?

#'@export
median_quart<-function(x,nround=NULL,probs=c(.25,.5,.75),
qtype=8,roundDig=2,drop0=FALSE,
groupvar=NULL,range=F,rangesep='ARRR',
rangearrow=' -> ',
prettynum=FALSE,.german=FALSE,.n=FALSE) {
  out <- ' '
  bigmark <- ifelse(.german,".",",")
  decimal <- ifelse(.german,",",".")
  if(length(na.omit(x))>=1) {
    if(is.null(groupvar)) {
      quart<-matrix(
        c(
          stats::quantile(x,probs=c(probs,0,1),na.rm=T,type=qtype),
          length(na.omit(x))),
          ncol=length(probs)+3)
    } else {
      quart<-matrix(
        unlist(
          by(x,groupvar,quantile,probs=c(probs,0,1),na.rm=T,
             type=qtype)),
        ncol=length(probs)+2,byrow=T)
      quart <- cbind(quart,
                     unlist(by(
                       x,groupvar,function(x){length(na.omit(x))})))
    }
    if(is.null(nround)) {
      quart[,]<-roundR(quart[,],
                                   level=roundDig,drop0=drop0,.german = .german)
      if(prettynum){
      #   quart <- apply(quart,1:2,function(x){
      #     formatC(as.numeric(x),
      #             digits = roundDig-1,
      #             format = 'f',
      #             big.mark = bigmark,
      #             decimal.mark = decimal,
      #             preserve.width = 'common',drop0trailing = F)})
      }
    } else {
      quart[,-ncol(quart)]<-round(quart[,-ncol(quart)],nround)
      if(prettynum){
        quart <- apply(quart,1:2,function(x){
          formatC(as.numeric(x),
                  digits = nround,
                  format = 'f',
                  big.mark = bigmark,
                  decimal.mark = decimal,
                  preserve.width = 'common',drop0trailing = F)})
      }
    }
    out<-str_glue('{quart[,2]} ({quart[,1]}/{quart[,3]})')
    if(range) {
      out<-str_glue('{out}{rangesep} [\\
                      {apply(matrix(quart[,(length(probs)+1):(length(probs)+2)],ncol=2),1,glue::glue_collapse,
                      sep=rangearrow)}]')
    }
    if(.n) {
      out<-str_glue('{out}{rangesep} [n={quart[,length(probs)+3]}]')
    }
  }
  return(out)
    }


# median_quart<-function(x,nround=NULL,probs=c(.25,.5,.75),
#                        qtype=8,roundDig=2,drop0=F,
#                        groupvar=NULL,range=F,rangesep='ARRR',
#                        rangearrow=' -> ') {
#   out <- ' '
#   if(length(na.omit(x))>0) {
#     if(is.null(groupvar)) {
#       quart<-matrix(
#         stats::quantile(x,probs=c(probs,0,1),na.rm=T,type=qtype),
#         ncol=length(probs)+2)
#     } else {
#       quart<-matrix(
#         unlist(
#           by(x,groupvar,quantile,probs=c(probs,0,1),na.rm=T,
#              librtype=qtype)),
#         ncol=length(probs)+2,byrow=T)
#     }
#     if(is.null(nround)) {
#       quart<-roundR(quart,level=roundDig,drop0=drop0)
#     } else {
#       quart<-round(quart,nround)
#     }
#     out<-glue::glue('{quart[,2]} ({quart[,1]}/{quart[,3]})')
#     if(range) {
#       out<-glue::glue('{out}{rangesep} [\\
#                   {apply(matrix(quart[,4:5],ncol=2),1,glue::glue_collapse,
#                         sep=rangearrow)}]')
#     }
#   }
#   return(out)
# }


#'Compute mean and se and put together with the +- symbol.
#'
#'@param x Data for computation.
#'@param roundDig Number of relevant digits for roundR.
#'@param drop0 Should trailing zeros be dropped?
#'@param mult multiplier for se, default 1, can be set to 2 or 1.96 to create confidence intervals
#'@export
meanse<-function(x,mult=1,roundDig=2,drop0=F) {
  m<-mean(x,na.rm=T)
  s<-sd(x,na.rm = T)/sqrt(length(na.omit(x)))
    ms <- roundR(c(m,s*mult),
                 level = roundDig,drop0 = drop0)
    out <- paste(ms[1],ms[2],sep='\u00B1')
  return(out)
}

#'@export
torso<-function(inputDF,rows2return=3) {
  if (is.data.frame(inputDF)|is.matrix(inputDF)) {
    rows<-nrow(inputDF)
    outputDF<-inputDF[c(1:rows2return,
                        sort(
                          sample((rows2return+1):(rows-rows2return),
                                 size=rows2return)),
                        (rows-rows2return+1):rows),]
  }
  if (is.vector(inputDF)) {
    rows<-length(inputDF)
    outputDF<-
      inputDF[c(1:rows2return,
                sort(
                  sample((rows2return+1):(rows-rows2return),
                         size=rows2return)),
                (rows-rows2return+1):rows)]
  }
  return(outputDF)
}

#'@export
se_median<-function(x) {
  mad(x,na.rm=T)/sqrt(length(na.omit(x)))
}

#'@export
median_cl_boot <- function(x, conf = 0.95, type='basic') {
  x <- na.omit(x)
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  # require(boot)
  bmedian <- function(x, ind) median(x[ind],na.rm=T)
  bt <- boot::boot(x, bmedian, 10000)
  bb <- boot::boot.ci(bt, type = type)
  data.frame(y = median(x,na.rm=T),
             ymin = quantile(bt$t, lconf),
             ymax = quantile(bt$t, uconf))
}

#'Compute absolute and relative frequencies.
#'
#'@param quelle Data for computation.
#'@param trenner delimiter between results per level. ARRR is my placeholder for later replacement with ^p (newline) in Word
#'@param return_level Should levels be reported?
#'@param ndigit Digits for rounding of relative frequencies.
#'@param percent logical, add percent-symbol after relative frequencies?
#'@param prettynum logical, apply prettyNum to results?
#'@param .german logical, should "." and "," be used as bigmark and decimal? Sets prettynum to TRUE
#'@export
cat_desc_stats<-function(quelle,trenner='ARRR',
                         return_level=TRUE,
                         ndigit=0,
                         groupvar=NULL,
                         singleline=FALSE,
                         percent=TRUE,
                         prettynum=FALSE,
                         .german=FALSE) {
  percent <- ifelse(percent,'%','')
  bigmark <- ifelse(.german,".",",")
  decimal <- ifelse(.german,",",".")
  if(!is.factor(quelle)) {
    # if(is.numeric(quelle)) {
    #   quelle<-factor(quelle,
    #                  levels=sort(unique(quelle)),
    #                  labels=sort(unique(quelle)))
    # } else {
    quelle<-factor(quelle)
  }
  level<- levels(quelle) %>% enframe(name=NULL)
  if(singleline){
    level <- paste(levels(quelle),sep = '',collapse = trenner)
  }
  if(is.null(groupvar)) {
    tableout<-matrix(table(quelle),
                     nrow=length(levels(quelle)),
                     byrow = F)
    colnames(tableout) <- 'abs'
    pt_temp <- round(100*prop.table(tableout),
                     ndigit)
    if(.german) {prettynum <- T}
    if(prettynum){
      pt_temp <- formatC(pt_temp,
                         digits = ndigit,
                         format = 'f',
                         big.mark = bigmark,
                         decimal.mark = decimal,
                         preserve.width = 'common',drop0trailing = F)
      tableout <- formatC(tableout,
                         digits = 0,
                         format = 'f',
                         big.mark = bigmark,
                         decimal.mark = decimal,
                         preserve.width = 'common')
    }
    ptableout<- matrix(paste0(' (',
                              pt_temp,
                              percent,')'),
                       nrow=length(levels(quelle)),
                       byrow = F)
    colnames(ptableout) <- 'rel'
  } else {
    tableout <- matrix(unlist(by(quelle,groupvar,table)),
                       nrow=length(levels(quelle)),
                       byrow = F)
    colnames(tableout) <- glue::glue('abs{levels(factor(groupvar))}')

    pt_temp <- round(100*prop.table(tableout,margin = 2),ndigit)
    if(prettynum){
      pt_temp <- formatC(pt_temp,
                         digits = ndigit,
                         format = 'f',
                         big.mark = bigmark,
                         decimal.mark = decimal,
                         preserve.width = 'common',drop0trailing = F)
      tableout <- formatC(tableout,
                          digits = 0,
                          format = 'f',
                          big.mark = bigmark,
                          decimal.mark = decimal,
                          preserve.width = 'common')
    }
    ptableout <- matrix(
      paste0(' (',pt_temp,
             percent,')'),
      nrow=length(levels(quelle)),
      byrow = F)
    colnames(ptableout) <- glue::glue('rel{levels(factor(groupvar))}')
  }
  zwert <- purrr::map2(tableout,ptableout,glue::glue) %>%
    as.character() %>%
    matrix(
      nrow=length(levels(quelle)),
      byrow = F) %>% as_tibble(.name_repair = 'minimal')
  if(is.null(groupvar)){
    colnames(zwert) <- 'desc'
  } else {
    colnames(zwert) <- glue::glue('desc{levels(factor(groupvar))}')
  }
  if(singleline){
    zwert <- map(zwert,
                 .f =  function(x)
                   glue::glue_collapse(x,sep = trenner)) %>%
      as_tibble()
  }
  levdesstats<-list(level=level, freq=zwert)
  if(return_level==T) {
    return(levdesstats)
  } else {
    return(zwert)
  }
}

#'@export
var_coeff<-function(x) {
  return(sd(x,na.rm=T)/mean(x,na.rm=T)*100)
}

#'@export
SEM <- function(x){
  return(sd(x,na.rm=T)/sqrt(length(na.omit(x))))
}
