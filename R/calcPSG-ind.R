#' Calculate plume direction and spread using indiviudal data on the natural scale
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return object of class "lm" from plume fit
#' @keywords
#' @export
#' @examples
#' calcPSG.ind(dat)

calcPSG.ind <- function(dat, plot = TRUE) {

  # Set initial values for gaussian fit estimation
  mu0 <- dat[which.max(CH4), wd3]
  k0 <- dat[,max(CH4)]

  # Fit Gaussian curve to wd bins
  wdfit <- nls(CH4 ~ k*exp( - 1/2*((wd3-mu)/sigma)^2),
                   start=c(mu = mu0, sigma=10, k = k0),
                   data=dat, algorithm = "port")
  # Plot Gaussian fit to wind direction
  if (plot==TRUE) {

    # Plot Gaussian Fit
    dat[,Fit := fitted(wdfit)]

    fig1 <- ggplot(dat, aes(x=wd3, y=CH4), col="black") +
      geom_point(alpha=0.2)+
      geom_point(aes(y=Fit), col="red") +
      geom_line(aes(y=Fit), col="red") +
      theme_bw(base_size=16) +
      xlab("Wind Direction") +
      ylab("CH4")
    print(fig1)
  }
  return(wdfit)
}

