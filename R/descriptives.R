#'Compute mean and sd and put together with the +- symbol.
#'
#'@param x Data for computation.
#'@param roundDig Number of relevant digigts for roundR.
#'@param drop0 Should trailing zeros be dropped?
#'@param groupvar Optional grouping variable for subgroups.
#'@param range Should min and max be included in output?
#'@param rangesep How should min/max be separated from mean+-sd?
#'Default ARRR is my shortcut for newline ^p later in Word.
#'@export
meansd<-function(x,roundDig=2,drop0=F,groupvar=NULL,
                 range=F,rangesep='ARRR') {
  out<-''
  if(length(na.omit(x))>0) {
    if(is.null(groupvar)) {
      meansd<-roundR(
        matrix(c(mean(x,na.rm = T),
                 sd(x,na.rm = T),
                 min(x,na.rm = T),
                 max(x,na.rm = T)),
               ncol=4,byrow = F),level=roundDig,
        drop0=drop0)
    } else {
      meansd<-roundR(
        matrix(c(by(x,groupvar,mean,na.rm=T),
                 by(x,groupvar,sd,na.rm=T),
                 by(x,groupvar,min,na.rm=T),
                 by(x,groupvar,max,na.rm=T)),
               ncol=4,byrow=F),level=roundDig,
        drop0=drop0)
    }
    out<-paste(meansd[,1],meansd[,2],sep='\u00B1')
    if(range) {
      out<-paste0(out,rangesep, ' [',
                  apply(matrix(meansd[,3:4],ncol=2),1,paste,
                        collapse=' -> '),']') #\u22ef
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
#'Default ARRR is my shortcut for newline ^p later in Word.
#'@export
median_quart<-function(x,nround=NULL,probs=c(.25,.5,.75),
                              qtype=8,roundDig=2,drop0=F,
                       groupvar=NULL,range=F,rangesep='ARRR') {
  out <- ' '
  if(length(na.omit(x))>0) {
    if(is.null(groupvar)) {
      quart<-matrix(
        stats::quantile(x,probs=c(probs,0,1),na.rm=T,type=qtype),
        ncol=length(probs)+2)
    } else {
      quart<-matrix(
        unlist(
          by(x,groupvar,quantile,probs=c(probs,0,1),na.rm=T,
             librtype=qtype)),
        ncol=length(probs)+2,byrow=T)
    }
    if(is.null(nround)) {
      quart<-roundR(quart,level=roundDig,drop0=drop0)
    } else {
      quart<-round(quart,nround)
    }
    out<-paste(quart[,2],' (',quart[,1],'/',quart[,3],')',sep='')
    if(range) {
      out<-paste0(out,rangesep,' [',
                  apply(matrix(quart[,4:5],ncol=2),1,paste,
                        collapse=' -> '),']')
    }
  }
  return(out)
}


#'@export
meanse<-function(x,mult=1) {
  m<-mean(x,na.rm=T)
  s<-plotrix::std.error(x,na.rm = T)
  out<-c(m,m-s*mult,m+s*mult)
  names(out)<-c('y','ymin','ymax')
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
#'@export
cat_desc_stats<-function(quelle,trenner='ARRR',
                         return_level=T,ndigit=0) {
  if(!is.factor(quelle)) {
    if(is.numeric(quelle)) {
      quelle<-factor(quelle,
                     levels=sort(unique(quelle)),
                     labels=sort(unique(quelle)))
    } else
    {quelle<-factor(quelle)}
  }
  tableout<-table(quelle)
  ptableout<- round(100*prop.table(tableout),ndigit)
  level<-paste(levels(quelle),collapse = trenner)

  #    level<-paste(names(tableout[1]))
  zwert<-paste0(tableout[1],' (',ptableout[1],'%)')

  if (length(tableout)>1) {
    for (var_j in 2:length(tableout)) {
      wert<-paste(paste0(tableout[var_j],' (',ptableout[var_j],'%)'),sep = trenner)
      zwert<-paste(zwert,wert,sep = trenner)
    }
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
  return(sd(x,na.rm=T)/mean(x,na.rm=T))
}
