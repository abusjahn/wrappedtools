#' Michaelis-Menten enzyme kinetics model and plot
#' 
#' \code{plot_MM} creates a Michaelis-Menten type Enzyme kinetics plot and returns model as well 
#'
#' @param substrate substrate concentration
#' @param velocity reaction velocity
#' @param xlab label for x-axis
#' @param ylab label for y-axis
#' @param title title of the plot
#'
#' @examples 
#' subst <- c(2.00, 1.00, 0.50, 0.25)
#' velo <- c(0.2253, 0.1795, 0.1380, 0.1000)
#' plot_MM(substrate = subst,velocity = velo)
#' 
#' @return
#' a list with elements "MMfit" and "MMplot"
#' 
#' @export
#'
plot_MM <- function(substrate, velocity, title = "Michaelis-Menten", 
                    xlab = "substrate", ylab = "velocity"){
  
  # Fitten des SSmicmen-Modell
  
  fit <- stats::nls(velocity ~ SSmicmen(substrate, Vm, K))
  
  # plotten des Fits
  
  MMplot <- ggplot(mapping = aes(x = substrate, y = velocity))+
    geom_point()+
    # stat_function(fun = function(sub){ (stats::coef(fit)[[1]] * sub) / ( stats::coef(fit)[[2]] + sub)}, color = "blue")+ # einzeichnen des Fitting
    geom_line(aes(x=seq(min(substrate),
                        max(substrate),
                        length.out=100),
                  y=predict(fit,
                            newdata = list(substrate=seq(min(substrate),
                                                   max(substrate),
                                                   length.out=100)))),
              color='blue')+
    geom_hline(yintercept = coef(fit)[[1]])+ # Vmax from coefficents 
    geom_text(aes(min(substrate, na.rm = T),stats::coef(fit)[[1]]),
              label = round(stats::coef(fit)[[1]], digits = 3), vjust = 1.2, hjust =  0)+ # Vmax aus coefficents eintragen
    geom_hline(yintercept =(stats::coef(fit)[[1]])/2,)+ # Vmax/2 from coefficents
    geom_text(aes(min(substrate, na.rm = T),
                  (stats::coef(fit)[[1]])/2),
              label = "Vmax/2", vjust = 1.4, hjust = .0)+ 
    geom_vline(xintercept =stats::coef(fit)[[2]])+ # Km from coefficents
    geom_text(aes(stats::coef(fit)[[2]],
                  min(velocity, na.rm = T)),
              label = round(stats::coef(fit)[[2]], digits = 3), 
              vjust = .5, hjust = -0.1)+ 
    xlab(xlab)+
    ylab(ylab)+
    ggtitle(title)
  
  return(list(MMfit=fit, MMplot=MMplot))
}

#' Lineweaver-Burk diagram
#'
#' \code{plot_LB} plots a Lineweaver-Burk diagram and computes the linear model
#' @param substrate substrate concentration
#' @param velocity enzyme velocity
#' @param title title of the plot
#' @param xlab label of the abscissa
#' @param ylab label of the ordinate
#'
#' @examples 
#' subst <- c(2.00, 1.00, 0.50, 0.25)
#' velo <- c(0.2253, 0.1795, 0.1380, 0.1000)
#' plot_LB(substrate = subst,velocity = velo)
#' 
#' @export
plot_LB <- function(substrate, velocity, 
                    title = "Lineweaver-Burk-Plot", xlab = "1/substrate", 
                    ylab = "1/velocity"){
  fit <- stats::lm(I(1/velocity)~I(1/substrate))
  LBplot <- ggplot(mapping = aes(x = 1/substrate,  y = 1/velocity))+
    geom_point()+
    geom_smooth(
      method = "lm",
      fullrange = TRUE
    )+
    # scale_x_continuous(expand=c(0,.1))+ #limits=c(0, max(1/sub+ 1))) +
    # scale_y_continuous(expand=c(0,.1))+ #limits=c(0, max(1/vel + .01))) +
    ggtitle(title)+
    xlab(xlab)+
    ylab(ylab) 
  return(list(LBfit=fit,LBplot=LBplot))
  #  Velo <-1/vel
  #  Subs <- 1/sub
  #  stats::coefficients(
  #    stats::lm(Velo~Subs)
  #  )
}
