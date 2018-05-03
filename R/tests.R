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

ksnormal<-function(ksdata)
{
  ksout<-ks.test(ksdata,'pnorm',mean(ksdata,na.rm=T),sd(ksdata,na.rm=T),exact=F)
  return(ksout)
}

