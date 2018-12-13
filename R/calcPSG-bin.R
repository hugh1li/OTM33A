#' Calculate plume direction and spread using binned data and natural scale
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return object of class "lm" with plume fit, has the side effect of adding column theta to dat
#' @export
#' @examples
#' calcPSG.bin(dat)

calcPSG.bin <- function(dat, Analyte="CH4", binwidth=2, wdfilt=180, min.n=0, plot = TRUE) {
  ## dat: data.table containing raw data, with header information in attribute fields
  ## binwidth: width of wind direction bins (in degrees)
  ## wdfilt: wind direction cut-off (degrees), e.g. 60 indicates only use
  ## data when the wind is coming from the direction of peak concentration
  dat = tryCatch({
  setnames(dat,Analyte,"Analyte")
  bins <- seq(-360, 360, binwidth)
  # Create columns in data.table with wind direction bin labels and centers
  dat[,wd.filter := ifelse(abs(wd3)<=wdfilt,TRUE,FALSE)]

  # Aggregate data by wd bins
  dat[,wdbin := cut(wd3, bins, right=FALSE)]
  ch4.wdbin <- dat[sub==TRUE&wd.filter==TRUE,list(Analyte = mean(Analyte),n = .N,wd3=mean(wd3)),wdbin][order(wdbin)]
  # check for consistency with ORD code
  ch4.wdbin[,n.filter := n > min.n*dat[,.N]]
  # Set initial values for gaussian fit estimation
  mu0 <- ch4.wdbin[which.max(Analyte), wd3]
  k0 <- ch4.wdbin[,max(Analyte)]
  # Fit Gaussian curve to wd bins
  wdfit <- nls(Analyte ~ k*exp( - 1/2*((wd3-mu)/sigma)^2),
                   start=c(mu = mu0, sigma=10, k = k0),
                   weights = n, dat=ch4.wdbin[n.filter==TRUE], algorithm = "port")
  setattr(dat,paste(Analyte,"wd.rot",sep="."),as.numeric(coef(wdfit)[1]))
  setattr(dat,paste(Analyte,"wd.sigma",sep="."),as.numeric(coef(wdfit)[2]))
  setattr(dat,paste(Analyte,"wd.peak",sep="."),as.numeric(coef(wdfit)[3]))
  # Plot Gaussian fit to wind direction
  if (plot==TRUE) {
    mm=unlist(dat[wd.filter==TRUE,list(min(wd3),max(wd3))])
    gauss.dat = data.frame(wd3=seq(-180,180,length=500))
    gauss.dat$Fit = predict(wdfit,gauss.dat)
    plot.title=attr(dat,"file.name")
    plot.title=ifelse(is.null(plot.title),"file.name not set",plot.title)
    # Plot Gaussian Fit

    fig1 <- ggplot(ch4.wdbin[n.filter==TRUE], aes(x=wd3, y=Analyte), col="black") +
      geom_point(alpha=0.2) +
      geom_line(data=gauss.dat,aes(x=wd3,y=Fit), col="red") +
      theme_bw(base_size=16) +
      xlab("Wind Direction") +
      ylab(Analyte) + ggtitle(plot.title)
    print(fig1)
  }
  return(dat)
  },finally={setnames(dat,"Analyte",Analyte)}
  )
}


