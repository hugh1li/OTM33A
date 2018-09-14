#' Calculate plume direction and spread using binned data and natural scale
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#' wdfilt:  number in degrees, used to exclude data from fit that are within the filter
#' @return object of class "lm" with plume fit,
#' has the side effect of adding columns thetabin, theta.filter, Ly to dat
#' @keywords
#' @export
#' @examples
#' calcLyProjection.bin(dat)

calcLyProjection.bin <- function(dat,wdfilt=90,binwidth=2,plot=TRUE) {
  # Calculate filter only include wind from the source +/- wind filter
  bins <- seq(-360, 360, binwidth)
  dat[,theta.filter := theta > -wdfilt & theta < wdfilt]
  bins <- seq(-360,360,binwidth)
  dat[,thetabin := cut(theta, bins, right=FALSE)]
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

  ch4.lybin <- dat[theta.filter==TRUE,list(CH4 = mean(CH4),n = .N,Ly=mean(Ly),theta=mean(theta)),thetabin][order(thetabin)]
  # return(ch4.lybin)
  # Set initial values for gaussian fit estimation
  mu0 <- ch4.lybin[which.max(CH4), Ly]
  a0 <- ch4.lybin[,max(CH4)]
  sigma0 <- ch4.lybin[,(max(Ly)-min(Ly))/5]
  start.0=list(mu=mu0,sigma=sigma0,a=a0)

  # Fit Gaussian curve to proxy distance
  lyfit <- nls(CH4 ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=ch4.lybin,
                  weights = n,
                  start=start.0,
                  algorithm = "port")
  if (plot == TRUE) {
    # Plot Gaussian Fit
    ch4.lybin[,Fit.Ly := fitted(lyfit)]
    fig2 <- ggplot(ch4.lybin, aes(x=Ly, y=CH4) ) +
      geom_point(col="black") +
      geom_line(aes(y=Fit.Ly), col="red") +
      theme_bw(base_size=16) +
      xlab("Ly") +
      ylab("CH4")
    print(fig2)
  }
  return(lyfit)
  setattr(dat,"gauss.peak",coef(lyfit)[3])
}
