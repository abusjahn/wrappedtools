ggcormat<-function(cor_mat, method='Correlation', title='',
                   maxpoint=2.1,textsize=5,axistextsize=2,
                   titlesize=3,breaklabels=NULL,
                   lower_only=T)
{
  rownames(cor_mat)<-colnames(cor_mat)
  # dd <- as.dist((1-cor_mat)/2)
  # hc <- hclust(dd)
  #cor_mat <-cor_mat[hc$order, hc$order]
  var_order<-rownames(cor_mat)
  if(lower_only){
    cor_mat[upper.tri(cor_mat)]<-NA
  }
  melted_cor_mat <- cor_mat %>% as_tibble() %>%
    mutate(Variable1=colnames(.)) %>%
    gather(key = Variable2,value = value, -Variable1) %>%
    na.omit() %>%
    mutate(Variable1=factor(Variable1,levels=var_order),
           Variable2=factor(Variable2,levels=var_order))
  # melted_cor_mat<-na.omit(reshape2::melt(cor_mat,
  #                                        varnames=c('Variable1','Variable2')))
  # melted_cor_mat$Variable1<-factor(melted_cor_mat$Variable1,
  #                                  levels=var_order,labels=var_order)
  # melted_cor_mat$Variable2<-factor(melted_cor_mat$Variable2,
  #                                  levels=var_order,labels=var_order)
  corvar_count<-nrow(cor_mat)
  if(lower_only){
    triangel<-data.frame(x=c(0,rep(corvar_count+2,2)),
                         y=c(rep(corvar_count+2,2),0))
    viereck1 <- data.frame(x=c(rep(0,2),rep(corvar_count+2.2,2)),
                           y=c(0,0.4,0.4,0))
    viereck2 <- data.frame(x=c(0,0.4,0.4,0),
                           y=c(rep(0,2),rep(corvar_count+2.2,2)))
    viereck3 <- data.frame(x=c(corvar_count+0.6,corvar_count+2.2,
                               corvar_count+2.2,corvar_count+0.6),
                           y=c(rep(0,2),rep(corvar_count+2.2,2)))
    viereck4 <- data.frame(x=c(rep(0,2),rep(corvar_count+2.2,2)),
                           y=c(corvar_count+0.6,corvar_count+2.2,
                               corvar_count+2.2,corvar_count+0.6))
  }
  if(is.null(breaklabels)) {
    breaklabels <- levels(melted_cor_mat$Variable1)
  }

  ggheatmap<-ggplot(melted_cor_mat,
                    aes(Variable2, Variable1))+
    #geom_tile(color = "white")+
    geom_point(aes(Variable2, Variable1, size=abs(value),color=value))+
    geom_vline(xintercept = seq(0.5,corvar_count+1, by=1),color='grey',size=.25)+
    geom_hline(yintercept = seq(0.5,corvar_count+1, by=1),color='grey',size=.25)+
    # geom_point(aes(Variable2, Variable1, size=abs(value),color=value))+
    scale_color_gradient2(low = "blue3", high = "red2", mid = "grey",
                          midpoint = 0, limit = c(-1,1), space = "Lab",
                          name=method) +
    scale_size_continuous(range=rel(c(.2,maxpoint)),guide=F)+
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 90, vjust = 0,
                                     hjust = 1),
          text = element_text(size = textsize),
          axis.text=element_text(size = rel(axistextsize)),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=rel(titlesize), face = 'bold'),
          legend.text = element_text(size=rel(1.25)),
          legend.title = element_text(size=rel(1.65))
          # legend.key.size = unit(.5,'lines'),
          # legend.key.width=unit(5.5,'cm'),
    )+
    coord_fixed()+
    # theme(legend.position = "right")+
    scale_y_discrete(limits = rev(levels(melted_cor_mat$Variable1)),
                     labels=rev(breaklabels))+
    scale_x_discrete(limits = levels(melted_cor_mat$Variable1),
                     labels=breaklabels)+
    ggtitle(title)



  if(lower_only){
    ggheatmap <- ggheatmap+
      guides(color = guide_colorbar(barwidth = 7, barheight = 1,
                                    title.position = "top", title.hjust = 0.5))+
      geom_polygon(data=triangel,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck1,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck2,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck3,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck4,aes(x=x,y=y),fill='white')+
      guides(color = guide_colorbar(barwidth = 7, barheight = 1,
                                    title.position = "top", title.hjust = 0.5))+
      theme(legend.justification = c(1, 1),
            legend.direction = "horizontal",
            legend.position = c(1, .99))

  }
  return(ggheatmap)
}
