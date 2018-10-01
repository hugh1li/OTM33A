#' Calculate plume direction and spread using binned data and natural scale
#' @param
#' dat: Data table, with names obtained from GMAP data output as of 2018-Aug
#'
#' wdfilt:  number in degrees, used to exclude data from fit that are within the filter
#' @return object of class "lm" with plume fit,
#' has the side effect of adding columns thetabin, theta.filter, Ly to dat
#' @export
#' @examples
#' calcLyProjection.bin(dat)

calcLyProjection.bin <- function(dat,thetafilt=90,binwidth=2,min.n=0,plot=TRUE) {
  # Calculate filter only include wind from the source +/- wind filter
  bins <- seq(-360, 360, binwidth)
  dat[,theta.filter := abs(theta) < thetafilt]
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

  ch4.lybin <- dat[theta.filter==TRUE&sub==TRUE,list(CH4 = mean(CH4),n = .N,Ly=mean(Ly),theta=mean(theta)),thetabin][order(thetabin)]
  # check for consistency with ORD code
  ch4.lybin[,n.filter := n > min.n*dat[,.N]]

  # return(ch4.lybin)
  # Set initial values for gaussian fit estimation
  mu0 <- ch4.lybin[which.max(CH4), Ly]
  a0 <- ch4.lybin[,max(CH4)]
  sigma0 <- ch4.lybin[,(max(Ly)-min(Ly))/5]
  start.0=list(mu=mu0,sigma=sigma0,a=a0)

  # Fit Gaussian curve to proxy distance
  lyfit <- nls(CH4 ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=ch4.lybin[n.filter==TRUE],
                  weights = n,
                  start=start.0,
                  algorithm = "port")
  if (plot == TRUE) {
    mm=unlist(dat[wd.filter==TRUE,list(min(wd3),max(wd3))])
    gauss.dat = data.frame(Ly=seq(-distance,distance,length=500))
    gauss.dat$Fit = predict(lyfit,gauss.dat)
    plot.title=attr(dat,"file.name")
    plot.title=ifelse(is.null(plot.title),"file.name not set",plot.title)
    # Plot Gaussian Fit
    fig2 <- ggplot(ch4.lybin[n.filter==TRUE], aes(x=Ly, y=CH4) ) +
      geom_point(col="black") +
      geom_line(data=gauss.dat,aes(y=Fit), col="red") +
      theme_bw(base_size=16) +
      xlab("Ly") +
      ylab("CH4") + ggtitle(plot.title)
    print(fig2)
  }
  setattr(dat,"Ly.rot",coef(lyfit)[1])
  setattr(dat,"Ly.sigma",coef(lyfit)[2])
  setattr(dat,"Ly.peak",coef(lyfit)[3])
  setattr(dat,"binwidth",binwidth)
  setattr(dat,"min.n",min.n)
  return(lyfit)
}
