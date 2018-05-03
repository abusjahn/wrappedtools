# basics
roundR <- function(roundin,smooth=F,level=2, textout=T,drop0=F){
  if (!is.matrix(roundin))
  {
    roundin<-matrix(roundin)
  }
  roundout<-roundin
  roundlevel<-0
  if(min(roundin,na.rm=T)!=0 | max(roundin,na.rm=T)!=0) {
    roundlevel<-round(max(0,level-log10(max(abs(roundin),
                                            na.rm=T))))
  }
  roundout[which(!is.na(roundout))]<-
    round(roundin[which(!is.na(roundin))],roundlevel)
  # if(all((roundin[which(!is.na(roundin))]-roundout[which(!is.na(roundout))])==0))
  # {
  #   roundlevel<-0
  #   roundout[which(!is.na(roundout))]<-
  #     round(roundin[which(!is.na(roundin))],roundlevel)
  # }
  if (smooth&max(abs(roundout),na.rm=T)!=0) {
    roundout[which(!is.na(roundout))]<-
      round(
        roundout[which(!is.na(roundout))]/
          10^round(log10(max(abs(roundout),na.rm=T))-level))*
      10^round(log10(max(abs(roundout),na.rm=T))-level)
  }
  if(textout==T) {
    roundout[which(!is.na(roundout))]<-
      formatC(roundout[which(!is.na(roundout))],format='f',
              digits=roundlevel,drop0trailing=drop0)
  }
  return(roundout)
}

markSign<-function(SignIn,plabel=c('n.s.','+','*','**','***')) {
  SignIn <- as.numeric(SignIn)
  SignOut<-' '
  if (!is.na(SignIn)) {
    SignOut<-plabel[1]
    if (SignIn<=0.1) {SignOut<-plabel[2]}
    if (SignIn<=0.0501) {SignOut<-plabel[3]}
    if (SignIn<=0.01) {SignOut<-plabel[4]}
    if (SignIn<=0.001) {SignOut<-plabel[5]}
  }
  return(SignOut)
}

formatP<-function(pIn,ndigits=3,text=T,pretext=F,mark=F) {
  formatp<-''
  if(is.numeric(pIn)) {
    if (!is.matrix(pIn)) {
      pIn<-matrix(pIn);
    }
    formatp<-apply(X=pIn,MARGIN=c(1,2),max,
                   10**(-ndigits),na.rm=F);
    formatp<-apply(X=formatp,MARGIN=c(1,2),round,ndigits);
    formatp<-apply(formatp,MARGIN=c(1,2),
                   formatC,format="f",
                   digits=ndigits,drop0trailing =F);
    if(pretext) {
      for (row_i in 1:nrow(pIn)) {
        for (col_i in 1:ncol(pIn)) {
          formatp[row_i,col_i]<-paste0(
            ifelse(pIn[row_i,col_i]<10**(-ndigits),
                   '<','='),
            formatp[row_i,col_i])
        }
      }
    }
    if(mark) {
      formatp<-matrix(
        paste(formatp,
              apply(gsub('[\\<\\=]','',formatp),c(1,2),markSign)),
        ncol=ncol(pIn))
    }
    if (text==F & pretext==F) {
      formatp<-apply(formatp,MARGIN=c(1,2),as.numeric)
    }
  }
  return(formatp);
}

DelEmptyCols<-function(df_in,minValid=1) {
  empties<-NA
  for (col_i in 1:ncol(df_in)) {
    if (length(na.omit(df_in[,col_i]))<minValid) {
      empties<-c(na.omit(empties),col_i)
    }
  }
  if (!is.na(empties[1]))
  {df_in<-df_in[,-empties]}
  return(df_in)
}

DelEmptyRows<-function(df_in,minValid=0,zero=F) {
  empties<-numeric(0)
  for (row_i in 1:nrow(df_in)) {
    if (!sum(!is.na(df_in[row_i,]))>minValid) {
      empties<-c(empties,row_i)
    }
  }
  if (zero) {
    empties<-numeric(0)
    for (row_i in 1:nrow(df_in)) {
      if (!sum(df_in[row_i,])>0) {
        empties<-c(empties,row_i)
      }
    }
  }
  if (!is.na(empties[1]))
  {df_in<-df_in[-empties,]}
  return(df_in)
}

FindVars<-function(varnames,allnames=colnames(rawdata),
                   exact=F,exclude=NA) {
  vars<-numeric()
  evars<-numeric()
  if (exact==T) {
    for (i in 1:length(varnames))
    {vars<-c(vars,grep(paste0('^',varnames[i],'$'),allnames))}
    return(unique(vars))
  } else {
    for (i in 1:length(varnames))
    {vars<-c(vars,grep(varnames[i],allnames))}
    vars<-sort(unique(vars))
    if(any(!is.na(exclude))) {
      for (i in 1:length(exclude))
      {evars<-c(evars,grep(exclude[i],allnames))}
      evars<-unique(na.omit(match(
        sort(unique(evars)),vars)))
      if(length(evars)>0)
      {vars<-vars[-evars]}
    }
    return(unique(vars))
  }
}

print_kable<-function(t,nrows=30,caption='',
                             ncols=100,...) {
  # require(knitr)
  for (block_i in 1:ceiling(nrow(t)/nrows)) {
    for (col_i in 1:ceiling((ncol(t)-1)/ncols)) {
      print(
        kable(
          t[(1+(block_i-1)*nrows):
              min(nrow(t),block_i*nrows),
            c(1,(2+(col_i-1)*ncols):min((1+col_i*ncols),ncol(t)))],
          row.names = F,
          caption = paste0(ifelse(block_i+col_i>2,'newpage continued: ',''),
                           caption,
                           '\n\n   ')))
    }
  }
}

cn<-function(data=rawdata) {
  colnames(data)
}

bt<-function(x) {
  return(paste0('`',x,'`'))
}
