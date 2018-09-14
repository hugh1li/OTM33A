#' Calculate plume direction and spread using indiviudal data and a lognormal fit
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return object of class "lm" with plume fit
#' @export
#' @examples
#' calcPSG.ind.log(dat)

calcPSG.ind.log <- function(dat, plot = TRUE) {
  ###############################################################################
  # Estimate Rate Function
  # Modified by Brad Venner based on code by Halley Brantley
  # 9 Aug 2018
  ###############################################################################

  ## dat: data.table containing raw data, with header information in attribute fields
  ## binwidth: width of wind direction bins (in degrees)
  ## wdfilt: wind direction cut-off (degrees), e.g. 60 indicates only use
  ## data when the wind is coming from the direction of peak concentration
  ## +/- 60
  ## datafolder: contains point source gaussian table
  # orig:  load(file.path(datafolder, "pgsigma.RData"))
  # mod:  psigma data loaded with package
  bins <- seq(-360, 360, binwidth)

  # Set initial values for gaussian fit estimation
  mu0 <- dat[which.max(CH4), wd3]
  k0 <- dat[,max(CH4)]
  bg <- attr(dat,"CH4.bg")
  # Fit Gaussian curve to wd bins
  wdfit <- nls(log(CH4.raw) ~ k -1/2*((wd3-mu)/sigma)^2,
                   start=c(mu = mu0, sigma=10, k = log(k0)),
                   data=dat, algorithm = "port")
  dat[,theta := wd3-coef(wdfit)[1]]

    # Plot Gaussian fit to wind direction
  if (plot==TRUE) {

    # Plot Gaussian Fit
    dat[,Fit := exp(fitted(wdfit))-bg]

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

