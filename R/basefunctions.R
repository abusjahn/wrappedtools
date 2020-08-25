#'Automatic rounding to a reasonable length, based on largest number
#'
#'\code{roundR} returns a matrix of rounded numbers.
#'
#'@param roundin A vector or matrix of numbers.
#'@param smooth A logical specifying if you want rounding before the dot
#'(e.g. 12345 to 12300).
#'@param level A number specifying number of relevant digits to keep.
#'@param textout A logical if output is converted to text.
#'@param drop0 A logical if trailing zeros should be dropped
#'@param .german A logical if german numbers should be reported
#'@export
roundR <- function(roundin,smooth=F,level=2, 
                   textout=T,drop0=F, .german=F){
  if(.german){textout <- T}
  decimalmark <- ifelse(.german,',','.')
  bigmark <- ifelse(.german,'.',',')
  if (!is.matrix(roundin))  {
    roundin<-matrix(as.numeric(roundin))
  }
  roundout<-roundin
  roundlevel<-0
  # if(.strict){
  #   roundlevel<-level
  #   roundout <- signif(roundin,digits = level)
  #   } else {
      if(min(roundin,na.rm=T)!=0 | max(roundin,na.rm=T)!=0) {
        roundlevel<-floor(max(0,level-log10(max(abs(roundin),
                                                na.rm=T))))
      }
      roundout[which(!is.na(roundout))]<-
        round(roundin[which(!is.na(roundin))],roundlevel)
    # }     # if(all((roundin[which(!is.na(roundin))]-roundout[which(!is.na(roundout))])==0))
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
  if(textout) {
    roundout[which(!is.na(roundout))]<-
      formatC(roundout[which(!is.na(roundout))],format='f',
              digits=roundlevel,drop0trailing=drop0,
              big.mark = bigmark,
              decimal.mark = decimalmark)
  }
  return(roundout)
}

#'Convert significance levels to symbols
#'
#'\code{markSign} returns a single text element.
#'
#'@param SignIn A single p-value.
#'@param plabel A translation table, predefined with the usual symbols
#'@export
markSign<-function(SignIn,plabel=c('n.s.','+','*','**','***')) {
  SignIn <- as.numeric(SignIn)
  SignOut <- cut(SignIn,breaks = c(-Inf,.001,.01,.05,.1,1),
      labels = rev(plabel))
  # SignOut<-' '
  # if (!is.na(SignIn)) {
  #   SignOut<-plabel[1]
  #   if (SignIn<=0.1) {SignOut<-plabel[2]}
  #   if (SignIn<=0.0501) {SignOut<-plabel[3]}
  #   if (SignIn<=0.01) {SignOut<-plabel[4]}
  #   if (SignIn<=0.001) {SignOut<-plabel[5]}
  # }
  return(SignOut)
  # cut(.1,c(0,.001,.01,.05,.1,1),c('***','**','*','+','n.s.'))
}

#'Re-format p-values
#'
#'\code{formatP} simplifies p-values by rounding to the maximum of p or a
#'predefined level. Optionally < or = can be added, as well as
#'symbols according to significance level.
#'
#'@param pIn A numeric vector or matrix with p-values.
#'@param ndigits Number of digits (default=3).
#'@param text Should output be casted to character default=T)?
#'@param pretext Should = or < be added before p (default=F)?
#'@param mark Should significance level be added after p (default=F)?
#'@param german_num change dot (default) to comma?
#'@export
formatP<-function(pIn,ndigits=3,text=T,pretext=F,mark=F,
                  german_num=F) {
  decimal.mark <- ifelse(german_num,',','.')
  formatp<-''
  if(is.numeric(pIn)) {
    if (!is.matrix(pIn)) {
      pIn<-matrix(pIn);
    }
    formatp<-apply(X=pIn,MARGIN=c(1,2),max,
                   10**(-ndigits),na.rm=F) %>%
      apply(MARGIN=c(1,2),round,ndigits) %>%
    apply(MARGIN=c(1,2),
                   formatC,format="f",
                   digits=ndigits,drop0trailing =F,
          decimal.mark=decimal.mark)
    if(pretext) {
      for (row_i in 1:nrow(pIn)) {
        for (col_i in 1:ncol(pIn)) {
          formatp[row_i,col_i]<-paste(
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

#'@export
DelEmptyCols<-function(df_in,minValid=1) {
  empties<-NA
  for (col_i in 1:ncol(df_in)) {
    if (df_in %>% pull(col_i) %>% na.omit() %>% length()<minValid) {
      empties<-c(na.omit(empties),col_i)
    }
  }
  if (!is.na(empties[1]))
  {df_in<-df_in[,-empties]}
  return(df_in)
}

#'@export
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

#'Find numeric index and names of column name patterns
#'
#'\code{FindVars} looks up columnames (by default for data-frame rawdata)
#'based on parts of names, using regular expressions.
#'exlusion rules may be specified as well.
#'
#'@param varnames Vector of pattern to look for.
#'@param allnames Vector of values to detect pattern in;
#'by default, colnames(rawdata).
#'@param exact Partial matching or exact only (adding ^ and $)?
#'@param exclude Vector of pattern to exclude from found names.
#'@param casesensitive Logical if case is respected in matching (default FALSE: a<>A)
#'@param fixed Logical, treat pattern as regex?
#'@export
#'@return A list with index, names, backticked names, and symbols
#'@examples
#'FindVars(varnames = c('^c','g'),allnames = colnames(mtcars))
#'FindVars(varnames = c('^c','g'),allnames = colnames(mtcars),exclude='r')
FindVars<-function(varnames,allnames=colnames(rawdata),
                   exact=F,exclude=NA,casesensitive=T,
                   fixed=F) {
  if(fixed) {exact <- F}
  allnames_tmp <- allnames
  if(!casesensitive){
    varnames <- tolower(varnames)
    allnames_tmp <- tolower(allnames)
    exclude <- tolower(exclude)
  }
  vars<-numeric()
  evars<-numeric()
  if (exact) {
    for (i in 1:length(varnames)) {
      vars<-c(vars,grep(paste0('^',varnames[i],'$'),allnames_tmp))
      }
    vars <- unique(vars)
  } else {
    for (i in 1:length(varnames)) {
      vars<-c(vars,grep(varnames[i],allnames_tmp,
                        fixed = fixed))
      }
    vars<-sort(unique(vars))
    if(any(!is.na(exclude))) {
      for (i in 1:length(exclude))
      {evars<-c(evars,grep(exclude[i],allnames_tmp))}
      evars<-unique(na.omit(match(
        sort(unique(evars)),vars)))
      if(length(evars)>0)
      {vars<-vars[-evars]}
    }
    vars <- unique(vars)
  }
  return(list(index=vars,
              names=allnames[vars],
              bticked=bt(allnames[vars]),
              symbols=rlang::syms(allnames[vars]),
              count=length(vars)))
}

#'Enhanced kable with definable number of rows/columns for splitting into subtables
#'
#'\code{print_kable} formats tibbles/df's for markdown
#'
#'@param t table to print
#'@param nrows number of rows (30) before splitting
#'@param ncols number of columns (100) before splitting
#'@param caption header

#'@export
print_kable<-function(t,nrows=30,caption='',
                             ncols=100,...) {
  # require(knitr)
  for (block_i in 1:ceiling(nrow(t)/nrows)) {
    for (col_i in 1:ceiling((ncol(t)-1)/ncols)) {
      if(block_i+col_i>2){cat('\\newpage\n\n')}
      print(
        knitr::kable(
          t[(1+(block_i-1)*nrows):
              min(nrow(t),block_i*nrows),
            c(1,(2+(col_i-1)*ncols):min((1+col_i*ncols),ncol(t)))],
          row.names = F,
          caption = paste0(ifelse(block_i+col_i>2,'continued: ',''),
                           caption,
                           '  \n  \n   ')))
      cat('  \n   \n')
    }
  }
}

#'Enhanced kable with latex
#'
#'\code{pdf_kable} formats tibbles/df's for markdown
#'
#'@param .input table to print
#'@param twidth default 14
#'@param innercaption subheader
#'@param caption header
#'@param foot footnote
#'@param escape see kable

#'@export
pdf_kable <- function(.input,width1=6,
                      twidth = 14,
                      tposition='left',
                      innercaption=NULL,
                      caption = '',
                      foot=NULL,
                      escape=T){
  ncols=ncol(.input)
  out <- kable(.input, format = "latex", booktabs = T,
               linesep = "",
               escape=escape, caption=caption,
               align = c('l',rep('c',ncols-1))) %>%
    kable_styling(position = tposition,
                  latex_options = c("striped",
                                    "hold_position")) %>%
    column_spec(-1, #border_left = T,
                width = paste0((twidth-width1)/(ncols-1),'cm'),
    ) %>%
    column_spec(1,bold = T,width = paste0(width1,'cm')) %>%
    row_spec(0, bold = T)
  if(!is.null(innercaption)){
    caption1 <- c(caption=ncols)
    names(caption1) <- caption
    out <- out %>%
      add_header_above(caption1,bold=T)
  }
  if(!is.null(foot)){
    out <- out %>%
      footnote(general = foot)
  }
  return(out)
}


#'Shortcut for colnames()
#'
#'\code{cn} lists colnames, by default for variable rawdata.
#'
#'@param data Data structure to read colnames from.
#'@export
cn<-function(data=rawdata) {
  colnames(data)
}

#'Add backticks to names or remove them
#'
#'\code{bt} adds leading and trailing backticks to make illegal variable names
#'usable. Optionally removes them.
#'
#'@param x Names to backtick
#'@param remove Option to remove existing backticks, default=F
#'@export
bt<-function(x,remove=F) {
  if(remove){
    return(gsub('`','',x))
  } else {
    return(paste0('`',x,'`'))
  }
}


#'Search within data.frame or tibble
#'
#' \code{tab.search} searches for pattern within a data-frame or tibble,
#' returning column(s) and row(s)
#'
#'@param searchdata table to search in, predefined as rawdata
#'@param pattern regex, for exact matches add ^findme$
#'@param find.all return all row indices or only 1st per column,default=TRUE
#'@param names.only return only vector of colnames rather than list with names and rows, default=FALSE
#'@export
tab.search <- function(searchdata=rawdata, pattern,
                       find.all = T,names.only=F)
{
  positions <- map(searchdata,str_which,pattern=pattern) %>% compact()
  if(!find.all) {
    positions <- map(positions,nth,n=1)
  }
  if(names.only){
    positions <- names(positions)
  }
  return(positions)
}


