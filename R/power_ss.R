#'Computation of sample size for traits with Poisson distribution
#'
#'@param .lambda_ctr expected count in control group.
#'@param .lamba_ctr_range Range for optimistic/pessimistic estimation for controls, resulting in three estimations.
#'@param .lambda_delta Expected difference on counts for treatment.
#'@param .lambda_delta_range Range for optimistic/pessimistic estimation for treatment. Within range, lambda is increased stepwise. 
#'@param .lambda_step defines stepwise increase.
#'@param .nrepl Number of replications for simulation.
#'@param .n_min Smallest sample size to start with.
#'@param .label_ctr Label for plot.
#'@param .label_verum Label for plot.
#'@export
pwr_poisson <- function(
  .lambda_ctr,
  .lambda_ctr_range=1,
  .lambda_delta,
  .lambda_delta_range=1,
  .lambda_step=.5,
  .nrepl=10^3,
  .n_min=4,
  .label_ctr='Lambda control',
  .label_verum='Lambda verum'){
  lambdas_ctr <- .lambda_ctr+
    (c(-.lambda_ctr_range, 0, .lambda_ctr_range))
  ssout_list <- list()
  for(ctr_i in 1:3){
    lambdas <- seq(
      ifelse(.lambda_delta<0,
             max(.lambda_step,
                 lambdas_ctr[ctr_i]+.lambda_delta-.lambda_delta_range),
             max(lambdas_ctr[ctr_i]+.lambda_step,
                 lambdas_ctr[ctr_i]+.lambda_delta-.lambda_delta_range)),
      ifelse(.lambda_delta<0,
             min(lambdas_ctr[ctr_i]+.lambda_delta+.lambda_delta_range,
                 lambdas_ctr[ctr_i]-.lambda_step),
             lambdas_ctr[ctr_i]+.lambda_delta+.lambda_delta_range),    
      # ifelse(.lambda_delta<0,
             .lambda_step)
    samplesize <- tibble(
      `lambda control`=rep(lambdas_ctr[ctr_i],length(lambdas)),
      `lambda verum`=lambdas,
      n=NA)
    for(l_i in seq_along(lambdas)){
      l_tmp <- lambdas[l_i]
      n_tmp <- .n_min
      n_step <- 1
      power_found <- F
      while(power_found==F){
        sign_count <- 0
        n_step <- cut(n_tmp,
                      c(1,rep(c(5,10),20)*rep(10^(1:20),each=2)),
                      c(rep(c(1,5),20)*10^rep(0:19, each=2)),
                      include.lowest = T) %>% 
          as.character() %>% as.numeric()
        # print(n_step)
        n_tmp=n_tmp+n_step
        # print(n_tmp)
        for(rep_i in seq_len(.nrepl)){
          if(wilcox.test(rpois(n_tmp,lambda = lambdas[l_i]),
                         rpois(n_tmp,lambdas_ctr[ctr_i]))$p.value<=.05){
            sign_count <- sign_count+1
          }
        }
        power_found=sign_count/.nrepl>.8  
      }
      samplesize$n[l_i] <- n_tmp
    }
    ssout_list  <- rlist::list.append(ssout_list,samplesize) 
  }
  samplesize <- map_df(ssout_list,rbind)  
  plottmp <- 
    samplesize %>% 
    mutate(`lambda control`=factor(`lambda control`)) %>% 
    ggplot(aes(`lambda verum`,n,
               color=`lambda control`))+
    # geom_smooth(se=F)+
    scale_color_discrete(name=.label_ctr)+
    geom_line()+
    geom_point(size=2,color='darkblue')+
    # scale_x_continuous(breaks=lambdas)+
    ggrepel::geom_label_repel(
      data=samplesize %>% dplyr::filter(`lambda control`==.lambda_ctr,
                                        `lambda verum`==.lambda_ctr+.lambda_delta),
      aes(label=n), color='black',
      hjust=1.1,vjust=-0.1)+
    scale_x_continuous(name = .label_verum,
                       breaks=seq(0,10^6,.lambda_step*2))+
    scale_y_log10(expand = expansion(mult=c(.05,.1)),
                  breaks = logrange_12357,
                  minor_breaks=logrange_123456789)
  return(list(samplesize=samplesize %>% 
                rename(!!sym(.label_ctr):=`lambda control`,
                       !!sym(.label_verum):=`lambda verum`),
              plot=plottmp))
}  




#'Computation of sample size for traits with ordinal distribution 
#'
#'@param .lambda_ctr expected count in control group.
#'@param .lamba_ctr_range Range for optimistic/pessimistic estimation for controls, resulting in three estimations.
#'@param .lambda_delta Expected difference on counts for treatment.
#'@param .lambda_delta_range Range for optimistic/pessimistic estimation for treatment. Within range, lambda is increased stepwise. 
#'@param .lambda_step defines stepwise increase.
#'@param .nrepl Number of replications for simulation.
#'@param .n_min Smallest sample size to start with.
#'@param .label_ctr Label for plot.
#'@param .label_verum Label for plot.
#'@export
pwr_ord <- function(
  .lambda_ctr,
  .lambda_ctr_range=1,
  .lambda_delta,
  .lambda_delta_range=1,
  .lambda_step=.5,
  .nrepl=10^3,
  .n_min=4,
  .label_ctr='Lambda control',
  .label_verum='Lambda verum'){
  lambdas_ctr <- .lambda_ctr+
    (c(-.lambda_ctr_range, 0, .lambda_ctr_range))
  ssout_list <- list()
  for(ctr_i in 1:3){
    lambdas <- seq(
      ifelse(.lambda_delta<0,
             max(.lambda_step,
                 lambdas_ctr[ctr_i]+.lambda_delta-.lambda_delta_range),
             max(lambdas_ctr[ctr_i]+.lambda_step,
                 lambdas_ctr[ctr_i]+.lambda_delta-.lambda_delta_range)),
      ifelse(.lambda_delta<0,
             min(lambdas_ctr[ctr_i]-.lambda_delta+.lambda_step,
                 lambdas_ctr[ctr_i]-.lambda_step),
             lambdas_ctr[ctr_i]+.lambda_delta+.lambda_delta_range),    
      ifelse(.lambda_delta<0,
             .lambda_step,.lambda_step))
    samplesize <- tibble(
      `lambda control`=rep(lambdas_ctr[ctr_i],length(lambdas)),
      `lambda verum`=lambdas,
      n=NA)
    for(l_i in seq_along(lambdas)){
      l_tmp <- lambdas[l_i]
      n_tmp <- .n_min
      n_step <- 1
      power_found <- F
      while(power_found==F){
        sign_count <- 0
        n_step <- cut(n_tmp,
                      c(1,rep(c(5,10),20)*rep(10^(1:20),each=2)),
                      c(rep(c(1,5),20)*10^rep(0:19, each=2)),
                      include.lowest = T) %>% 
          as.character() %>% as.numeric()
        # print(n_step)
        n_tmp=n_tmp+n_step
        # print(n_tmp)
        for(rep_i in seq_len(.nrepl)){
          if(wilcox.test(rpois(n_tmp,lambda = lambdas[l_i]),
                         rpois(n_tmp,lambdas_ctr[ctr_i]))$p.value<=.05){
            sign_count <- sign_count+1
          }
        }
        power_found=sign_count/.nrepl>.8  
      }
      samplesize$n[l_i] <- n_tmp
    }
    ssout_list  <- rlist::list.append(ssout_list,samplesize) 
  }
  samplesize <- map_df(ssout_list,rbind)  
  plottmp <- 
    samplesize %>% 
    mutate(`lambda control`=factor(`lambda control`)) %>% 
    ggplot(aes(`lambda verum`,n,
               color=`lambda control`))+
    # geom_smooth(se=F)+
    scale_color_discrete(name=.label_ctr)+
    geom_line()+
    geom_point(size=2,color='darkblue')+
    # scale_x_continuous(breaks=lambdas)+
    ggrepel::geom_label_repel(
      data=samplesize %>% dplyr::filter(`lambda control`==.lambda_ctr,
                                        `lambda verum`==.lambda_ctr+.lambda_delta),
      aes(label=n), color='black',
      hjust=1.1,vjust=-0.1)+
    scale_x_continuous(name = .label_verum,
                       breaks=seq(0,10^6,.lambda_step*2))+
    scale_y_log10(expand = expansion(mult=c(.05,.1)),
                  breaks = logrange_12357,
                  minor_breaks=logrange_123456789)
  return(list(samplesize=samplesize %>% 
                rename(!!sym(.label_ctr):=`lambda control`,
                       !!sym(.label_verum):=`lambda verum`),
              plot=plottmp))
}  



# library(plotrix)
# library(pwr)
# library(ggplot2)
# source('F:/Aktenschrank/Analysen/R/EigeneFunktionen.R')
#logo<-png::readPNG('C:/Users/abusj/Pictures/HealthTwiSt_txt_200neu2.png')
# logo<-png::readPNG('C:/Users/abusj/Pictures/HealthTwiSt_final_AB400.png')
# g<-rasterGrob(logo, interpolate=TRUE)

#'@export
FZcalc_t <- function(SD.estim, SD.worst, SD.best,SD.step,
                     treat='Treatment',outcome='Outcome',
                     d.estim, d.worst, d.best, d.step,
                     testtype='two.sample',p=.05,power=.8,
                     tail='two.sided',dropOut1=10,dropOut2=0) {
  #    tail<-tail
  
  ylabel='n per group'
  Estimates <- matrix(c(SD.estim,d.worst,NA,NA,NA,
                        SD.estim,d.estim,NA,NA,NA,
                        SD.estim,d.best,NA,NA,NA),
                      nrow=5,ncol=3,
                      dimnames=list(c('SD','Difference','n',
                                      'n incl.10% dropout',
                                      paste0('n incl.',dropOut2,'% dropout')),
                                    c('minimal','expected','optimistic')))
  for (count in 1:3)  {
    temp <-power.t.test(
      n=NULL, delta = Estimates[2,count],sd=Estimates[1,count],
      sig.level=p,power=power, alternative=tail,
      type=testtype) #two.sample / paired
    Estimates[3,count] <- ceiling(temp$n)
    Estimates[4,count] <- ceiling(Estimates[3,count]/(100-dropOut1)*100)
    Estimates[5,count] <- ceiling(Estimates[3,count]/(100-dropOut2)*100)
  }
  d.abs.range <- seq(d.worst,d.best,d.step)	#absolute Differenz
  n.range <- rep(NA,length(d.abs.range))
  SD.range <- seq(SD.worst, SD.best, SD.step)
  ymax <- power.t.test(
    n=NULL, delta = d.worst/SD.worst,sig.level=p,power=power,
                       alternative=tail, type=testtype)$n*1.2#2*max(Estimates[4,])
  power_df<-data.frame(SD=rep(SD.range,each=length(d.abs.range)),
                       delta=rep(d.abs.range,length(SD.range)),
                       delta_sd=NA,n=NA,n_dropout1=NA,n_dropout2=NA)
  power_df$delta_sd<-round(power_df$delta/power_df$SD,3)
  for (SD.count in 1:length(SD.range))
  {
    d.range <- d.abs.range/SD.range[SD.count]			#Differenz in SD-Einheiten
    
    for (d.count in 1:length(d.range)) {
      temp <- power.t.test(n=NULL, delta = d.range[d.count],
                           sig.level=p,power=power,
                           alternative=tail, type=testtype)
      n.range[d.count] <- ceiling(temp$n)
      power_df$n[(SD.count-1)*length(d.abs.range)+d.count]<-ceiling(temp$n)
    }
  }
  power_df$n_dropout1<-ceiling(power_df$n/(100-dropOut1)*100)
  power_df$n_dropout2<-ceiling(power_df$n/(100-dropOut2)*100)
  power_df$SD<-factor(power_df$SD)
  powerplot<-ggplot(power_df,aes(x=delta,y=n,linetype=SD))+
    scale_size_manual(values=c(3,5,3))+
    geom_segment(x=d.worst,xend=d.estim,
                 y=power_df$n[which(
                   power_df$SD==SD.estim &
                     as.character(power_df$delta)==as.character(d.estim))],
                 yend=power_df$n[which(
                   power_df$SD==SD.estim &
                     as.character(power_df$delta)==as.character(d.estim))],
                 color='darkgreen',linetype=3, size=1.5)+
    geom_segment(x=d.estim,
                 xend=d.estim,
                 y=min(power_df$n),
                 yend=power_df$n[which(
                   power_df$SD==SD.estim &
                     as.character(power_df$delta)==as.character(d.estim))],
                 color='darkgreen',linetype=3, size=1.5)+
    geom_point(aes(size=SD),color='blue')+
    scale_linetype_manual(values=c(2,1,3))+
    geom_line(aes(group=SD))+
    ggtitle(paste(treat,"->",outcome,sep=' '))+
    ylab('n per group')+xlab('absolute difference')+
    annotate(geom = 'text',x = d.best, y=max(power_df$n),
             label=paste0('Power=',power,', p=',p,',',tail,', ',testtype,
                          '\n SD=',SD.estim,
                          '  (',SD.best,'..',SD.worst,')',
                          ' Difference=',
                          d.estim,' Effectsize=',round(d.estim/SD.estim,2),
                          '\nn=',
                          power_df$n[
                            which(power_df$SD==SD.estim &
                                    as.character(power_df$delta)==
                                    as.character(d.estim))],
                          '\nn incl. ',dropOut1,'% drop-out=',
                          power_df$n_dropout1[
                            which(power_df$SD==SD.estim &
                                    as.character(power_df$delta)==
                                    as.character(d.estim))],
                          ifelse(dropOut2>0,paste0(
                            '\nn incl. ',dropOut2,'% drop-out=',
                            power_df$n_dropout2[
                              which(power_df$SD==SD.estim &
                                      as.character(power_df$delta)==
                                      as.character(d.estim))]),
                            '')),
             hjust=1, vjust=1,size=4)+
    annotate(geom='text', x = d.worst+.05*(d.best-d.worst),y=min(power_df$n),
             label=paste0('\u00A9 HealthTwiSt ',Sys.Date()),
             hjust=0, vjust=0,color='darkgray',size=3)#+
    # annotation_custom(g, xmin=d.worst-.05*(d.best-d.worst),
    #                   xmax=d.worst+.05*(d.best-d.worst),
    #                   ymin=min(power_df$n)-.05*(max(power_df$n)-min(power_df$n)),
    #                   ymax=min(power_df$n)+.05*(max(power_df$n)-min(power_df$n)))
  return(list(plot=powerplot,table=power_df,
              n=power_df[which(power_df$SD==SD.estim &
                                 power_df$delta==d.estim),
                         c('n','n_dropout1','n_dropout2')]))
}
# Example:
#  FZcalc_t(SD.estim=1,SD.worst=1.1,SD.best=.9,SD.step=-.1,treat='Med',outcome='RR',d.estim=.8,d.worst=.5,d.best=1.2,d.step=.1)

FZcalc_Prop <- function(pControl,pMax,pMin,pStep,pEstim,
                        treat='Treatment',outcome='Outcome',
                        tail='two.sided',p=0.05,power=.8,patyears=1)
{
  ylabel='n per group'
  Estimates <- matrix(c(pControl,pMin,NA,NA,NA,
                        pControl,pEstim,NA,NA,NA,
                        pControl,pMax,NA,NA,NA), nrow=5,ncol=3,byrow=F,
                      dimnames=list(c('p Control','p Cases','n','n incl.10% dropout','n incl.20% dropout'),
                                    c('minimal','expected','optimistic')))
  for (count in 1:3)
  {
    temp <-power.prop.test(n=NULL,
                           p1=min(1-(1-Estimates[1,count])^patyears,
                                  1-(1-Estimates[2,count])^patyears),
                           p2=max(1-(1-Estimates[1,count])^patyears,
                                  1-(1-Estimates[2,count])^patyears),
                           sig.level=p,power=power, alternative=tail) #two.sample / paired
    Estimates[3,count] <- ceiling(temp$n)
    Estimates[4,count] <- ceiling(temp$n/90*100)
    Estimates[5,count] <- ceiling(temp$n/80*100)
  }
  d.abs.range <- seq(pMin,pMax,pStep)	#absolute Differenz
  n.range <- rep(NA,length(d.abs.range))
  ymax <- max(power.prop.test(n=NULL, p1=1-(1-pControl)^patyears,
                              p2=1-(1-pMin)^patyears,
                              sig.level=p,power=power, alternative=tail)$n,
              power.prop.test(n=NULL, p1=1-(1-pControl)^patyears,
                              p2=1-(1-pMax)^patyears,
                              sig.level=p,power=power, alternative=tail)$n)*1.2
  
  #    py<-ifelse(patyears==1,'','2 Jahre Beobachtung')
  plot(0,0, main=paste(treat,"->",outcome,sep=' '), ylab=ylabel,
       col.main='darkblue', pch=15, col='blue', xlab='Proportion in Cases',
       cex.axis=1, family='sans', font.lab=2, cex=1, ylim=c(0,ymax),
       xlim=c(pMax, pMin), xaxp=c(pMin,pMax,round((pMax-pMin)/pStep)))
  
  d.range <- d.abs.range
  for (d.count in 1:length(d.range))  {
    n.range[d.count] <- ceiling(power.prop.test(
      n=NULL, p1=1-(1-pControl)^patyears,p2=1-(1-d.range[d.count])^patyears,
      sig.level=p,power=power, alternative=tail)$n)
  }
  plotcolor<-'blue'
  plotsymbol <- 16
  plotline=1
  #    lines(c(pEstim,pEstim),c(0,ymax),col='green', lwd=3,lty=3)
  #    lines(c(0,max(d.abs.range)),c(Estimates[3,2],Estimates[3,2]),col='green', lwd=3,lty=3)
  lines(c(pEstim,pEstim),c(0,Estimates[3,2]),col='green', lwd=3,lty=3)
  lines(c(pEstim,max(d.abs.range)),c(Estimates[3,2],Estimates[3,2]),col='green', lwd=3,lty=3)
  legend(max(d.abs.range),ymax,paste0('Power=',power,', p=',p,', ',tail,
                                      ' p Control=',pControl,', p Cases=',
                                      pEstim),
         xjust=0, yjust=1, cex=1, bg='white',box.col='white')
  # legend(max(d.abs.range),ymax*.9,
  #        paste0('p Control=',pControl,', p Cases=',pEstim),
  #               # ', patyears=',patyears),
  #        xjust=0, yjust=1, cex=1, bg='white',box.col='white')
  legend(max(d.abs.range),ymax*.88,
         paste0('n=',Estimates[3,2],', n incl. 10% drop-out=',Estimates[4,2]),
         xjust=0, yjust=1,
         cex=1, bg='white',box.col='white')
  # legend(max(d.abs.range),ymax*.82,
  #        paste('n incl. 10% drop-out=',Estimates[4,2],sep=''),
  #        xjust=0, yjust=1, cex=1, bg='white',box.col='white')
  legend(max(d.abs.range),ymax*.76,
         paste('n incl. 20% drop-out=',Estimates[5,2],sep=''),xjust=0, yjust=1,
         cex=1, bg='white',box.col='white')
  legend(max(d.abs.range),1,paste('HealthTwiSt',Sys.Date(),sep=' '),
         xjust=0, yjust=0.5, cex=0.7, bg='white',box.col='white') #adj=c(0,0), )
  points(d.abs.range,n.range,pch=plotsymbol,col=plotcolor)
  lines(d.abs.range,n.range,col='grey',lty=plotline)
  return(Estimates)
}

pwr.t.test.prop<-function(prop,nmax=1000,d,alternative,sig.level)
{
  n1<-1
  power<-0
  while(n1<=nmax&power<.8)
  {
    n1<-n1+1
    n2=ceiling(n1/prop*(1-prop))
    power<-pwr.t2n.test(n1=n1,n2,d,alternative=alternative,sig.level=sig.level)$power
    #       print(paste(n1,n2,power))
  }
  return(c(n1,n2))
}

FZcalc_t_prop <- function(SD.estim, SD.worst, SD.best,SD.step,
                          treat='Treatment',outcome='Outcome',
                          d.estim, d.worst, d.best, d.step, alternative='t',
                          p=.05,power=.8,type='two.sample',prop=.5)
{
  ylabel='n (total sample)'
  Estimates <- matrix(c(SD.estim,d.worst,rep(NA,6),
                        SD.estim,d.estim,rep(NA,6),
                        SD.estim,d.best,rep(NA,6)), nrow=8,ncol=3,
                      dimnames=list(c('SD','Difference','n1',
                                      'n1 incl.10% dropout','n1 incl.20% dropout',
                                      'n2','n2 incl.10% dropout','n2 incl.20% dropout'),
                                    c('minimal','expected','optimistic')))
  for (count in 1:3)
  {
    temp <-pwr.t.test.prop(prop,d=Estimates[2,count]/Estimates[1,count],
                           sig.level=p,alternative=alternative)
    Estimates[3,count] <- temp[1]
    Estimates[4,count] <- ceiling(temp[1]/90*100)
    Estimates[5,count] <- ceiling(temp[1]/80*100)
    Estimates[6,count] <- ceiling(temp[2])
    Estimates[7,count] <- ceiling(temp[2]/90*100)
    Estimates[8,count] <- ceiling(temp[2]/80*100)
  }
  
  d.abs.range <- seq(d.worst,d.best,d.step)	#absolute Differenz
  n.range <- rep(NA,length(d.abs.range))
  SD.range <- seq(SD.worst, SD.best, SD.step)
  
  ymax <- sum(pwr.t.test.prop(prop,d=d.worst/SD.worst,sig.level=p,
                              alternative=alternative))*1.2
  #      pwr.t.test(n=NULL, d=d.worst/SD.worst,sig.level=p,power=0.8,
  #                      alternative=alternative,type=type)$n*2.4#2*max(Estimates[4,])
  plot(0,0, main=paste(treat,"->",outcome,sep=' '), ylab=ylabel,
       col.main='darkblue', pch=15, col='blue', xlab='absolute Difference',
       cex.lab=1.5,cex.axis=1.3, family='sans', font.lab=2, cex=2, ylim=c(0,ymax),
       xlim=c(d.worst-(d.step*3), d.best),
       xaxp=c(d.worst,d.best,round((d.best-d.worst)/d.step)))
  
  for (SD.count in 1:length(SD.range))
  {
    d.range <- d.abs.range/SD.range[SD.count]			#Differenz in SD-Einheiten
    for (d.count in 1:length(d.range))
    {
      temp <- pwr.t.test.prop(prop,d=d.range[d.count],sig.level=p,
                              alternative=alternative)
      n.range[d.count] <- sum(temp)
    }
    plotcolor<-'blue'
    plotsymbol<-20
    plotline=3
    if (SD.range[SD.count]==SD.estim)
    {
      plotcolor<-'blue'
      plotsymbol <- 16
      plotline=1
      lines(c(d.estim,d.estim),c(0,Estimates[3,2]+Estimates[6,2]),col='green', lwd=3,lty=3)
      #          lines(c(d.estim,0),c(Estimates[3,2],Estimates[3,2]),col='green', lwd=3,lty=3)
      #          lines(c(d.estim,d.estim),c(0,ymax),col='green', lwd=3,lty=3)
      lines(c(0,d.estim),c(Estimates[3,2]+Estimates[6,2],
                           Estimates[3,2]+Estimates[6,2]),
            col='green', lwd=3,lty=3)
      legend(max(d.abs.range),ymax,paste('Power=',power,', p=',p),xjust=1, yjust=1,
             cex=1.3, bg='white',box.col='white')
      legend(max(d.abs.range),ymax*.92,
             paste(' SD=',SD.estim,' (',SD.best,'..',SD.worst,
                   ') Difference=',d.estim,sep=''),
             xjust=1, yjust=1, cex=1.3, bg='white',box.col='white')
      legend(max(d.abs.range),ymax*.84,
             paste(' n=',Estimates[3,2]+Estimates[6,2],
                   ' (',Estimates[3,2],'/',Estimates[6,2],')',sep=''),
             xjust=1, yjust=1, cex=1.3, bg='white',box.col='white')
      legend(max(d.abs.range),ymax*.76,paste('n incl. 10% drop-out=',
                                             Estimates[4,2]+Estimates[7,2],
                                             ' (',Estimates[4,2],'/',Estimates[7,2],')',sep=''),
             xjust=1, yjust=1, cex=1.3, bg='white',box.col='white')
      legend(max(d.abs.range),ymax*.68,paste('n incl. 20% drop-out=',
                                             Estimates[5,2]+Estimates[8,2],
                                             ' (',Estimates[5,2],'/',
                                             Estimates[8,2],')',sep=''),
             xjust=1, yjust=1, cex=1.3, bg='white',box.col='white')
      legend(min(d.abs.range)-d.step,1,paste('HealthTwiSt',Sys.Date(),sep=' '),
             xjust=0, yjust=0.5, cex=0.7, bg='white',box.col='white') #adj=c(0,0), )
    }
    points(d.abs.range,n.range,pch=plotsymbol,col=plotcolor,cex=1.5)
    lines(d.abs.range,n.range,col='grey',lty=plotline,lwd=2)
    text(d.worst-(d.step*3),max(n.range),
         paste('SD=',SD.range[SD.count],sep=''),cex=1.2,adj=c(0,0.5))
  }
  return(Estimates)
}


# Sample size and optimal designs for reliability studies.
# Walter SD, Eliasziw M, Donner A.
# Stat Med. 1998 Jan 15;17(1):101-10.
pwr.ICC<-function(nRepl=2,rho0=0,rho1,alpha=.05,beta=.2,dropout=.1)
{
  Ua<-qnorm(1-alpha)
  Ub<-qnorm(1-beta)
  C0<-(1+nRepl*(rho0/(1-rho0)))/(1+nRepl*(rho1/(1-rho1)))
  const<-ifelse(nRepl==2,1.5,1)
  k<-const+(2*(Ua+Ub)^2*nRepl)/(log(C0)^2*(nRepl-1))
  nSample<-ceiling(k)
  nSampleDO<-ceiling(nSample/(1-dropout))
  return(list(n=nSample,nInclDropout=nSampleDO))
}
# pwr.ICC(rho0=.8,rho1=.95,alpha=.025)
# Stat Med. 1998 Jan 15;17(1):101-10.
# Sample size and optimal designs for reliability studies.
# Walter SD1, Eliasziw M, Donner A.
pwr.r<-function(r1=NA,n=NA,alpha=.05,beta=.2,dropout=.1,tails=2)
{
  Ua<-qnorm(1-alpha/tails)
  Ub<-qnorm(1-beta)
  Cr<-log((1+r1)/(1-r1))/2
  const<-3
  if(is.na(beta))
  {
    Cr<-log((1+r1)/(1-r1))/2
    Ub<-(n-const)^.5*Cr-Ua
    beta<-pnorm(Ub,lower.tail=F)
  }
  if(is.na(n))
  {
    Cr<-log((1+r1)/(1-r1))/2
    Ub<-qnorm(1-beta)
    n<-const+((Ua+Ub)/Cr)^2
  }
  if(is.na(r1))
  {
    r1<-.01
    test<-exp(2*(Ua+Ub)/(n-const)^.5)
    while ((1+r1)/(1-r1)<test)
    {r1<-r1+.01}
  }
  nSampleDO<-ceiling(n/(1-dropout))
  return(list(r=r1,
              n=ceiling(n),nInclDropout=nSampleDO,
              dropout=dropout,
              alpha=alpha,
              power=round(1-beta,3)))
}
# pwr.r(r1=.4,tails=1)
# pwr.r(r1=.4,beta=NA,n=47)
# pwr.r(n=17)

#simulation
# mean1 <- 26
# sd1 <- 24
# mean_exp <- 15
# mean_best <- 10
# mean_worst <- 20
# mean_step <- 1
# mean_range <- seq(mean_best,mean_worst,mean_step)
# n_range <- seq(10,300,5)
# n_simul <- 10^3
# power_out <- 
#   tibble(
#     mean2=mean_range,
#     n=NA)
# last_ni=1
# for(mean_i in seq_along(mean_range)){
#   print(paste('mean',mean_range[mean_i]))
#   for(n_i in last_ni:length(n_range)) {
#     print(n_range[n_i])
#     pcounter <- 0
#     for(sim_i in 1:n_simul) {
#       p <- wilcox.test(rnorm(n_range[n_i],
#                              mean1,sd1),
#                        rnorm(n_range[n_i],
#                              mean_range[mean_i],sd1))$p.value
#       if(p<=.05) {pcounter <- pcounter+1}
#     }
#     if(pcounter/n_simul>=.8) {
#       power_out$n[mean_i] <- n_range[n_i]
#       last_ni <- n_i-1
#       break()
#     }
#   }
# }
# n_estim <- power_out %>% 
#   dplyr::filter(mean2==mean_exp) %>% 
#   select(n) %>% unlist()
# power_out %>% ggplot(aes(mean2,n))+
#   geom_point()+geom_line()+
#   geom_segment(x=mean_exp,xend=mean_exp,
#                y=0,yend=n_estim,linetype=2)+
#   geom_segment(x=mean_best,xend=mean_exp,
#                y=n_estim,yend=n_estim,linetype=2)+
#   scale_x_continuous(breaks=mean_range)+
#   # scale_y_continuous(breaks=n_range)+
#   annotate('text',mean_best,max(power_out$n,na.rm=T),
#            hjust=0,vjust=1,
#            label=paste('Power Wilcoxon test',
#                        '\nreference =',mean1,'\u00B1',sd1,
#                        '\nn per group = ',n_estim,
#                        '\nwith 10% drop-out:',
#                        ceiling(n_estim/9*10),
#                        '\nwith 20% drop-out:',
#                        ceiling(n_estim/8*10)))
# 
# 
