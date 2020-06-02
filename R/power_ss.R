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
                  breaks = logrange_15,
                  minor_breaks=logrange_123456789)
  return(list(samplesize=samplesize,
              plot=plottmp))
}  