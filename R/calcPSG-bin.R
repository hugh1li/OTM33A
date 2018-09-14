#' Calculate plume direction and spread using binned data and natural scale
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return object of class "lm" with plume fit, has the side effect of adding column theta to dat
#' @keywords
#' @export
#' @examples
#' calcPSG.bin(dat)

calcPSG.bin <- function(dat, binwidth=2, wdfilt=180, plot = TRUE) {
  ## dat: data.table containing raw data, with header information in attribute fields
  ## binwidth: width of wind direction bins (in degrees)
  ## wdfilt: wind direction cut-off (degrees), e.g. 60 indicates only use
  ## data when the wind is coming from the direction of peak concentration
  ## +/- 60
  ## datafolder: contains point source gaussian table
  # orig:  load(file.path(datafolder, "pgsigma.RData"))
  # mod:  psigma data loaded with package
  bins <- seq(-360, 360, binwidth)
  # Create data.frame with wind direction bin labels and centers
  # orig  bins.df <- subset(data.frame(wdbin=cut(bins, bins, right=FALSE),
  #                             thetabin=cut(bins, bins, right=FALSE),
  #                             center=bins+binwidth/2), !is.na(wdbin))
  # mod
  dat[,time_int := seq(1, .N, 1)]

  # Aggregate data by wd bins
  dat[,wdbin := cut(dat$wd3, bins, right=FALSE)]
  ch4.wdbin <- dat[,list(CH4 = mean(CH4),n = .N,wd3=mean(wd3)),wdbin][order(wdbin)]

  # Set initial values for gaussian fit estimation
  mu0 <- ch4.wdbin[which.max(CH4), wd3]
  k0 <- ch4.wdbin[,max(CH4)]

  # Fit Gaussian curve to wd bins
  wdfit <- nls(CH4 ~ k*exp( - 1/2*((wd3-mu)/sigma)^2),
                   start=c(mu = mu0, sigma=10, k = k0),
                   weights = n, dat=ch4.wdbin, algorithm = "port")
  dat[,theta := wd3-coef(wdfit)[1]]
  # Plot Gaussian fit to wind direction
  if (plot==TRUE) {

    # Plot Gaussian Fit
    ch4.wdbin[,Fit := fitted(wdfit)]

    fig1 <- ggplot(ch4.wdbin, aes(x=wd3, y=CH4), col="black") +
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

