#' Michaelis-Menten enzyme kinetics model and plot
#' 
#' \code{plot_MM} creates a Michaelis-Menten type Enzyme kinetics plot and returns model as well 
#'
#' @param data data structure with columns for model data
#' @param substrate colname for substrate concentration
#' @param velocity colname for reaction velocity
#' @param group colname for optional grouping factor
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param title title of the plot
#'
#' @examples 
#' MMdata <- data.frame(subst = c(2.00, 1.00, 0.50, 0.25),
#'                  velo = c(0.2253, 0.1795, 0.1380, 0.1000))
#'                  
#' plot_MM(data=MMdata,
#'         substrate = 'subst',velocity = 'velo')
#' 
#' MMdata <- data.frame(subst = rep(c(2.00, 1.00, 0.50, 0.25),2),
#'                  velo = c(0.2253, 0.1795, 0.1380, 0.1000,
#'                           0.4731333, 0.4089333, 0.3473000, 0.2546667),
#'                  condition = rep(c('C1','C2'),each=4))
#'                  
#' plot_MM(data=MMdata,substrate = 'subst',
#'         velocity = 'velo',group='condition')
#' 
#' @return
#' a list with elements "MMfit" and "MMplot"
#' 
#' @export
#'
plot_MM <- function(
    data,  
    substrate, velocity, group = NULL,
    title = "Michaelis-Menten", 
    xlab = "substrate", ylab = "velocity"){
  if(is.null(group)) {
    group <- 'Group'  
    data[[group]] <- 'all data'
  }
  fit_list <- list()
  plot_list <- list()
  groups <- data[[group]] |> as.factor() |> levels()
  for(group_i in seq_along(groups)){
    # Fitten des SSmicmen-Modell
    groupdata <- 
      data |> 
      filter(!!rlang::sym(group)==groups[group_i])
    vdata <- groupdata[[velocity]]
    sdata <- groupdata[[substrate]]
    fit <- stats::nls(vdata ~ SSmicmen(sdata, Vm, K)) 
      
    linedata <- tibble(
      x=seq(0,#min(groupdata[[substrate]]),
          max(groupdata[[substrate]])*1.5,
          length.out=100))
    linedata$y <- 
      predict(fit,
              newdata = list(sdata=linedata$x))
    refdata <- broom::tidy(fit) |> 
      select('term','estimate') |> 
    pivot_wider(names_from = 'term',values_from = 'estimate') |> 
      mutate(xv=max(sdata, na.rm = T),
             yk= min(vdata, na.rm = T))
    # plotten des Fits
    
    MMplot <- ggplot(data=groupdata,
                     mapping = aes(x = .data[[substrate]],#sdata, 
                                   y = .data[[velocity]]))+
      geom_point()+
      geom_line(data=linedata,aes(x=.data[['x']], y=.data[['y']]),
                color='blue')+
      geom_hline(data = refdata,
                 aes(yintercept = .data[['Vm']]), linetype=3)+ # Vmax from coefficents 
      geom_text(data = refdata,
                aes(.data[['xv']],.data[['Vm']],
                label = paste(
                  'Vmax =',
                  roundR(.data[['Vm']]))),
                vjust = 1.4, hjust =  1)+ # Vmax from coefficents 
      geom_hline(data = refdata,
                 aes(yintercept =.data[['Vm']]/2),linetype=3)+ # Vmax/2 from coefficents
      geom_text(data = refdata,
                aes(.data[['xv']],.data[['Vm']]/2),
                label = "Vmax/2", vjust = -0.8, hjust = 1)+ 
      geom_vline(data = refdata,
                 aes(xintercept=.data[['K']]), linetype=2)+ # Km from coefficents
      geom_text(data = refdata,
                aes(.data[['K']],.data[['yk']],
                label = paste(
                  'K =',
                  roundR(.data[['K']]))), 
                vjust = .5, hjust = -0.1)+ 
      scale_x_continuous(n.breaks = 10)+
      xlab(xlab)+
      ylab(ylab)+
      ggtitle(title)
    if(length(groups)>1){
      MMplot <- 
        MMplot+
        labs(subtitle = groups[group_i])
    }
    fit_list <- rlist::list.append(fit_list,fit)
    rm(fit)
    names(fit_list)[length(fit_list)] <- groups[group_i]
    
    plot_list <- rlist::list.append(plot_list,MMplot)
    rm(MMplot)
    names(plot_list)[length(plot_list)] <- groups[group_i]
  }    
  return(list(MMfit=fit_list, MMplot=plot_list))
}

#' Lineweaver-Burk diagram
#'
#' \code{plot_LB} plots a Lineweaver-Burk diagram and computes the linear model
#' @param data data structure with columns for model data
#' @param substrate colname for substrate concentration
#' @param velocity colname for reaction velocity
#' @param group colname for optional grouping factor
#' @param title title of the plot
#' @param xlab label of the abscissa
#' @param ylab label of the ordinate
#'
#' @examples 
#' MMdata <- data.frame(subst = c(2.00, 1.00, 0.50, 0.25),
#'                  velo = c(0.2253, 0.1795, 0.1380, 0.1000))
#'                  
#' plot_LB(data=MMdata,
#'         substrate = 'subst',velocity = 'velo')
#' 
#' MMdata <- data.frame(subst = rep(c(2.00, 1.00, 0.50, 0.25),2),
#'                  velo = c(0.2253, 0.1795, 0.1380, 0.1000,
#'                           0.4731333, 0.4089333, 0.3473000, 0.2546667),
#'                  condition = rep(c('C1','C2'),each=4))
#'                  
#' plot_LB(data=MMdata,substrate = 'subst',
#'         velocity = 'velo',group='condition')
#' 
#' @export
plot_LB <- function(data,  
                    substrate, velocity, group = NULL,
                    title = "Lineweaver-Burk-Plot", xlab = "1/substrate", 
                    ylab = "1/velocity"){
  if(is.null(group)) {
    group <- 'Group'  
    data[[group]] <- 'all data'
  }
  fit_list <- list()
  plot_list <- list()
  groups <- data[[group]] |> as.factor() |> levels()
  for(group_i in seq_along(groups)){
    groupdata <- 
      data |> 
      filter(!!sym(group)==groups[group_i])
    # vdata <- groupdata[[velocity]]
    # sdata <- groupdata[[substrate]]
    fitformula=paste0('I(1/',bt(velocity),') ~ I(1/',bt(substrate),')') |> 
      as.formula()
    fit <- stats::lm(fitformula, data=groupdata)
  LBplot <- ggplot(data = groupdata,
                   mapping = aes(x = 1/.data[[substrate]],
                                 y = 1/.data[[velocity]]))+
    geom_point()+
    geom_smooth(
      method = "lm",
      fullrange = TRUE
    )+
    scale_x_continuous(n.breaks = 10)+
    ggtitle(title)+
    xlab(xlab)+
    ylab(ylab) 
  if(length(groups)>1){
    LBplot <- 
      LBplot+
      labs(subtitle = groups[group_i])
  }
  fit_list <- rlist::list.append(fit_list,fit)
  rm(fit)
  names(fit_list)[length(fit_list)] <- groups[group_i]
  
  plot_list <- rlist::list.append(plot_list,LBplot)
  rm(LBplot)
  names(plot_list)[length(plot_list)] <- groups[group_i]
  }
  return(list(LBfit=fit_list,LBplot=plot_list))
  #  Velo <-1/vel
  #  Subs <- 1/sub
  #  stats::coefficients(
  #    stats::lm(Velo~Subs)
  #  )
}
