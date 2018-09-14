#' Calculate plume direction and spread along axis perpendicular to the wind using individual data
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return
#'   object of class "lm" with plume fit
#' @export
#' @examples
#' calcLyProjection.ind(dat)

calcLyProjection.ind <- function(dat,wdfilt=90,plot=TRUE) {
  # Calculate filter only include wind from the source +/- wind filter
  dat[,theta.filter := theta > -wdfilt & theta < wdfilt]

  distance <- attr(dat,"distance")
  dat[,Ly := distance*sin(theta*pi/180)]
  # ch4.thetabin1 <- ddply(subset(dat.filt, !is.na(thetabin)), .(thetabin), summarize,
  #                    ch4 = mean(ch4L),
  #                    n = length(ch4L)
  #                    )

  # Add bin center to aggregated values
  # ch4.thetabin <- merge(subset(ch4.thetabin1, n > 1), bins.df)

  # Calculate proxy distance (based on wd) along axis perpendicular to
  # wind direction (Ly)
  # ch4.thetabin$Ly <- distance*sin(ch4.thetabin$center*pi/180)

  # return(ch4.lybin)
  # Set initial values for gaussian fit estimation
  mu0 <- dat[which.max(CH4), Ly]
  a0 <- dat[,max(CH4)]
  sigma0 <- dat[,(max(Ly)-min(Ly))/5]
  start.0=list(mu=mu0,sigma=sigma0,a=a0)

  # Fit Gaussian curve to proxy distance
  lyfit <- nls(CH4 ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=dat,
                  start=start.0,
                  algorithm = "port")
  if (plot == TRUE) {
    # Plot Gaussian Fit
    dat[,Fit.Ly := fitted(lyfit)]
    fig2 <- ggplot(dat, aes(x=Ly, y=CH4) ) +
      geom_point(col="black") +
      geom_line(aes(y=Fit.Ly), col="red") +
      theme_bw(base_size=16) +
      xlab("Ly") +
      ylab("CH4")
    print(fig2)
  }
  return(lyfit)
}
