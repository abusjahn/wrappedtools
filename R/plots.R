#'Print graphical representation of a correlation matrix.
#'
#'@param cor_mat correlation matrix as produced by cor.
#'@param method text specifying type of correlation.
#'@param title plottitle.
#'@param maxpoint maximum for scale_size_manual, may need adjustment depending on plotsize.
#'@param textsize for theme text.
#'@param axistextsize relativ text size for axes.
#'@param titlesize as you already guessed, relative text size for title.
#'@param breaklabels currently not used, intended for str_wrap.
#'@param .low Color for heatmap.
#'@param .high Color for heatmap.
#'@param lower_only should only lower triangle be plotted?
#'@param .legendtitle Optional name for color legend.
#'@param p_mat Optional matrix of p-values; if provided, this is used to define size of dots rather than absolute correlation
#'@export
ggcormat<-function(cor_mat, p_mat=NULL,
                   method='Correlation', title='',
                   maxpoint=2.1,textsize=5,axistextsize=2,
                   titlesize=3,breaklabels=NULL,
                   lower_only=T,
                   .low="blue3",.high="red2",
                   .legendtitle=NULL)
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
           Variable2=factor(Variable2,levels=var_order),
           value=as.numeric(value),
           size=abs(value))
  corvar_count<-nrow(cor_mat)
  if(!is.null(p_mat)){
    melted_p_mat <-  p_mat %>% as_tibble() %>%
      mutate(Variable1=colnames(.)) %>%
      gather(key = Variable2,value = size, -Variable1) %>%
      mutate(Variable1=factor(Variable1,levels=var_order),
             Variable2=factor(Variable2,levels=var_order),
             size=-log10(formatP(as.numeric(size),
                                 text = F))) %>% 
      na.omit() 
    melted_p_mat$size[which(melted_p_mat$size>3)] <- 3
  melted_cor_mat <- full_join(melted_cor_mat[1:3],melted_p_mat,
                              by=c('Variable1','Variable2'))
    }
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
    geom_point(aes(Variable2, Variable1, 
                   size=size,color=value))+
    geom_vline(xintercept = seq(0.5,corvar_count+1, by=1),color='grey',size=.25)+
    geom_hline(yintercept = seq(0.5,corvar_count+1, by=1),color='grey',size=.25)+
    # geom_point(aes(Variable2, Variable1, size=abs(value),color=value))+
    scale_color_gradient2(low = .low, high = .high, mid = "grey",
                          midpoint = 0, limit = c(-1,1), space = "Lab",
                          name=method) +
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
  if(is.null(p_mat)){
    ggheatmap <- ggheatmap+
      scale_size_continuous(name = ifelse(
        is.null(.legendtitle),'abs. correlation scaling',
        .legendtitle),
    limits = c(0,1),
    breaks=c(.1,.5,.9),
    labels=c(.1,.5,.9),
    range=rel(c(.2,maxpoint)))
  } else{
    ggheatmap <- ggheatmap+
      scale_size_continuous(name = ifelse(
        is.null(.legendtitle),'p-value scaling',
        .legendtitle),
        limits = c(0,3),
        breaks=c(1,2,3),
        labels=c(.1,.01,.001),
        range=rel(c(.2,maxpoint)))
    
  }
  
  
  
  if(lower_only){
    ggheatmap <- ggheatmap+
      geom_polygon(data=triangel,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck1,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck2,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck3,aes(x=x,y=y),fill='white')+
      geom_polygon(data=viereck4,aes(x=x,y=y),fill='white')+
      guides(color = guide_colorbar(barwidth = 7, barheight = 1,
                                    title.position = "top", 
                                    title.hjust = 0.5, order=2),
             size=guide_legend(order=1,
                               title.position='top'))+
      theme(legend.justification = c('right', 'top'),
            legend.direction = "horizontal",
            legend.position = c(1, .99))
    
  }
  return(ggheatmap)
}

#'Plot  a regression tree with ggplot.
#'
#'@param rpartdata output from rpart.
#'@param miny ??.
#'@param title Headline for plot.
#'@param german Use German numbers.
#'@export
gg_rtree<-function(rpartdata=rpart_out,miny=NULL,
                   title='',german=F)
{
  yesno <- c('yes','no')
  splitlevel=attr(rpartdata,'xlevels')
  if(german){yesno <- c('ja','nein')}
  t<-ggdendro::dendro_data(rpartdata,uniform=T,compress=T)
  if(is.numeric(t$leaf_labels$label)) {
    t$leaf_labels$label<-prettyNum(as.numeric(t$leaf_labels$label))
  }
  if(german){
    t$leaf_labels$label <- str_replace(t$leaf_labels$label,'\\.',',')
  }
  t$leaf_labels<-dplyr::arrange(t$leaf_labels,x)
  if(is.null(miny)) {miny<-c(min(t$leaf_labels$y),min(t$leaf_labels$y)/2)}
  for(label_i in seq_along(splitlevel)){
    t$labels$label[which(str_detect(t$labels$label,names(splitlevel)[label_i]))] %<>% 
      str_replace_all(c('=a'=str_glue('={splitlevel[[label_i]][1]}'),
                        '=b'=str_glue('={splitlevel[[label_i]][2]}')))
  }
  t$labels$label<-enc2utf8(as.character(t$labels$label))
  t$labels$label<-stringr::str_replace(pattern='>=',replacement=' \u2265 ',
                                       string=gsub('<',' <',t$labels$label))
  if(german){
    t$labels$label<-stringr::str_replace(pattern='\\.',replacement=',',
                                         string=gsub('<',' <',t$labels$label))
  }
  t$segments$leaf<-yesno[2]
  for (i in 1:nrow(t$segments)) {
    if(t$segments$y[i]>t$segments$yend[i])
    {
      temp<-t$segments$y[i]
      t$segments$y[i]<-t$segments$yend[i]
      t$segments$yend[i]<-temp
    }
    
  }
  for(i in 1:nrow(t$leaf_labels)) {
    t$segments$y[which(t$segments$x==t$leaf_labels$x[i] &
                         t$segments$y==t$leaf_labels$y[i])]<-
      ifelse(i%%2==0,miny[1],miny[2])
    t$leaf_labels$y[i]<-
      ifelse(i%%2==0,miny[1],miny[2])
    t$segments$leaf[which(t$segments$x==t$leaf_labels$x[i] &
                            t$segments$y==t$leaf_labels$y[i])]<-yesno[1]
  }
  #yes/no edges
  t$yes<-t$segments[which(t$segments$y==t$segments$yend),1:3]
  t$yes$label<-yesno[1]
  t$no<-t$segments[which(t$segments$y==t$segments$yend),c(1,3:4)]
  colnames(t$no)[3]<-'x'
  t$no$label<-yesno[2]
  
  # t$leaf_labels$y<-c(min(t$leaf_labels$y),0.05)
  tree<-ggplot(t$segments)+
    ggtitle(title)+
    geom_segment(aes(x,y,xend=xend,yend=yend,
                     linetype=leaf,size=leaf))+
    scale_size_manual(values=c(.5,1.25),guide=F)+
    scale_linetype_manual(values=c(1,2),guide=F)+
    geom_label(data=t$labels,
                        aes(x,y,
                                     label=gsub('>=',' \u2265 ',label)),
               vjust=-.25,hjust=.5,fontface='bold')+
    geom_label(data=t$yes,aes(x,y,label=label),vjust=1,size=3,
               label.r=unit(.5,units='lines'),color='white',fill='black',
               fontface='bold')+
    geom_label(data=t$no,aes(x,y,label=label),vjust=1,size=3,
               label.r=unit(.5,units='lines'),color='white',fill='black',
               fontface='bold')+
    geom_label(data=t$leaf_labels,aes(x,y,label=label),vjust=1,
               label.r=unit(0,'lines'),fontface='bold',fill='lightgrey')+
    scale_y_continuous(limits=c(0,max(t$labels$y)*1.1))+
    ggdendro::theme_dendro()+
    theme(plot.title = element_text(hjust = 0.5,size=rel(2)))
  return(tree)
}


#'Create alluvialplot.
#'
#'\code{alluvialplot} returns a ggplot object.
#'@param .data tibble or data_frame with data
#'@param .x column defining x-axis
#'@param .label optional x-axis label
#'@param .fill optional column for subgroups
#'@param .title optional plot title
#'@param .gridrow optional facetting variable
#'@param .gridcol optional facetting variable
#'@param  .fillcolors values manually defining fill colors.
#'@export
alluvialplot<-function(.data,.x,.label=NULL,.fill=NULL,
                       .title=NULL,.gridrow=NULL,.gridcol=NULL,
                       .fillcolors=NULL)
{
  if(is.null(.label)){
    .label <- .x
  }
  plottmp <- eval(parse(text=paste0(
    'ggplot(data = .data,',
    'aes(y=n,',
    paste0('axis',1:length(.x),'=',.x,collapse = ','),
    '))')))+
    ggalluvial::geom_alluvium(aes_string(fill=.fill))+
    ggalluvial::geom_stratum(width = 1/3, fill = "darkgrey", color = "lightgrey") +
    geom_label(stat = "stratum", aes(label=after_stat(stratum)))+#infer.label = TRUE) +
    scale_x_discrete(limits = .label,
                     expand = c(.1, .05)) +
    # scale_fill_viridis_d(option = 'D',guide=F)+
    scale_y_continuous()+
    theme(panel.grid.major.x = element_blank())
  if(!is.null(.title)) {
    plottmp  <- plottmp +
      ggtitle(.title)
  }
  
  if(!is.null(.fillcolors)){
    plottmp <- plottmp+
      scale_fill_manual(values = .fillcolors,guide=F)
  } else {
    plottmp <- plottmp+
      scale_fill_viridis_d(option = 'D',guide=F)
  }
  if(!is.null(.gridrow)){
    plottmp <- plottmp  +
      facet_grid(rows=.gridrow, cols = NULL,scales = 'free', 
                 labeller = label_both, switch = 'y')
  }
  return(plottmp)
}

