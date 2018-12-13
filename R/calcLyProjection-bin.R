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

calcLyProjection.bin <- function(dat,Analyte="CH4",thetafilt=90,binwidth=2,min.n=0,plot=TRUE) {
  # Calculate filter only include wind from the source +/- wind filter
  dat <- tryCatch({
    setnames(dat,Analyte,"Analyte")
    rotation <- attr(dat,paste(Analyte,"wd.rot",sep="."))
    bins <- seq(-360, 360, binwidth)
    dat[,theta := wd3-rotation]
    dat[,theta.filter := abs(theta) < thetafilt]
    bins <- seq(-360,360,binwidth)
    dat[,thetabin := cut(theta, bins, right=FALSE)]
    distance <- attr(dat,"distance")
    dat[,Ly := distance*sin(theta*pi/180)]

    lybin <- dat[theta.filter==TRUE&sub==TRUE,list(Analyte = mean(Analyte),n = .N,Ly=mean(Ly),theta=mean(theta)),thetabin][order(thetabin)]
    # check for consistency with ORD code
    lybin[,n.filter := n > min.n*dat[,.N]]

    # return(ch4.lybin)
    # Set initial values for gaussian fit estimation
    mu0 <- lybin[which.max(Analyte), Ly]
    a0 <- lybin[,max(Analyte)]
    sigma0 <- lybin[,(max(Ly)-min(Ly))/5]
    start.0=list(mu=mu0,sigma=sigma0,a=a0)

    # Fit Gaussian curve to proxy distance
    lyfit <- nls(Analyte ~ a*exp( - 1/2*((Ly-mu)/sigma)^2),
                  data=lybin[n.filter==TRUE],
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
      fig2 <- ggplot(lybin[n.filter==TRUE], aes(x=Ly, y=Analyte) ) +
        geom_point(col="black") +
        geom_line(data=gauss.dat,aes(y=Fit), col="red") +
        theme_bw(base_size=16) +
        xlab("Ly") +
        ylab(Analyte) + ggtitle(plot.title)
      print(fig2)
    }
    setattr(dat,paste(Analyte,"Ly.rot",sep="."),as.numeric(coef(lyfit)[1]))
    setattr(dat,paste(Analyte,"Ly.sigma",sep="."),as.numeric(coef(lyfit)[2]))
    setattr(dat,paste(Analyte,"Ly.peak",sep="."),as.numeric(coef(lyfit)[3]))
    setattr(dat,"binwidth",binwidth)
    setattr(dat,"min.n",min.n)
    return(dat)
  },
  finally={setnames(dat,"Analyte",Analyte)})
}
