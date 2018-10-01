###############################################################################
# Estimate Rate Function
# Halley Brantley
# 10/01/15
###############################################################################
library(plyr)
library(pracma)
library(ggplot2)
###############################################################################

calcPSG <- function(STR, binwidth, wdfilt, datafolder, plot = TRUE) {
  ## STR: STR file loaded using import STR, includes STR$dat which contains
  ## concentration time series, and STR$runinfo that contains the
  ## distance from the soure, height of the source, height of the sensor
  ## and true release rate
  ## binwidth: width of wind direction bins (in degrees)
  ## wdfilt: wind direction cut-off (degrees), e.g. 60 indicates only use
  ## data when the wind is coming from the direction of peak concentration
  ## +/- 60
  ## datafolder: contains point source gaussian table

  load(file.path(datafolder, "pgsigma.RData"))
  bins <- seq(-360, 360, binwidth)
  # Create data.frame with wind direction bin labels and centers
  bins.df <- subset(data.frame(wdbin=cut(bins, bins, right=FALSE),
                               thetabin=cut(bins, bins, right=FALSE),
                               center=bins+binwidth/2), !is.na(wdbin))
  dat <- STR$dat
  distance <- STR$runinfo[, "distance"]
  sensor_ht <- STR$runinfo[, "sensor_ht"]
  source_ht <- STR$runinfo[, "source_ht"]
  release_rate <- STR$runinfo[, "release_rate"]
  dat$time_int <- seq(1, nrow(dat), 1)

  # Aggregate data by wd bins
  dat$wdbin <- cut(dat$wd3, bins, right=FALSE)
  ch4.wdbin1 <- ddply(subset(dat, !is.na(wdbin)), .(wdbin), summarize,
                      ch4 = mean(ch4L),
                      n = length(ch4L))
  # Add bin center to aggregated values
  ch4.wdbin <- merge(subset(ch4.wdbin1, n> 0.01*nrow(dat)), bins.df)

  # Set initial values for gaussian fit estimation
  mu0 <- ch4.wdbin[which(ch4.wdbin$ch4==max(ch4.wdbin$ch4)), "center"]
  k0 <- max(ch4.wdbin$ch4)

  # Fit Gaussian curve to wd bins
  wdfit <- summary(nls(ch4 ~ k*exp(-1/2*(center-mu)^2/(sigma^2)),
                       start=c(mu = mu0, sigma=10, k = k0),
                       weights = n, dat=ch4.wdbin, algorithm = "port"))

  # Center wind direction plume peak
  dat$theta <- dat$wd3 - wdfit$coeff[1]
  dat$theta <- ifelse(dat$theta > 180, dat$theta-360, dat$theta)
  dat$theta <- ifelse(dat$theta < -180, dat$theta+360, dat$theta)

  # Plot Gaussian fit to wind direction
  if (plot==TRUE) {
    mu.wd <- wdfit$coeff[1, 1]
    sigma2.wd <- (wdfit$coeff[2, 1])^2
    a.wd <- wdfit$coeff[3, 1]

    # Plot Gaussian Fit
    ch4.wdbin$preds <- a.wd*
      exp(-0.5 * ((ch4.wdbin$center - mu.wd)^2/sigma2.wd))

   fig1 <- ggplot(ch4.wdbin, aes(x=center, y=ch4), col="black") +
      geom_point(data=dat, aes(x=wd3, y=ch4L), alpha=0.2)+
      geom_point() +
      geom_point(aes(y=preds), col="red") +
      geom_line(aes(y=preds), col="red") +
      theme_bw(base_size=16) +
      xlab("Wind Direction") +
      ylab("CH4 above background")
  print(fig1)
  }
  ###############################################################################
  # Subset file to only include wind from the source +/- wind filter
  dat.filt <- subset(dat, theta > -wdfilt & theta < wdfilt)
  dat.filt$thetabin <- cut(dat.filt$theta, bins, right=FALSE)

  ch4.thetabin1 <- ddply(subset(dat.filt, !is.na(thetabin)), .(thetabin), summarize,
                      ch4 = mean(ch4L),
                      n = length(ch4L)
                      )

  # Add bin center to aggregated values
  ch4.thetabin <- merge(subset(ch4.thetabin1, n > 1), bins.df)

  # Calculate proxy distance (based on wd) along axis perpendicular to
  # wind direction (Ly)
  ch4.thetabin$Ly <- distance*sin(ch4.thetabin$center*pi/180)
  dat.filt$Ly <- distance*sin(dat.filt$theta*pi/180)

  # Fit Gaussian curve to proxy distance
  # set initial values
  sigma0 <- (max(ch4.thetabin$Ly)-min(ch4.thetabin$Ly))/5
  mu0 <- ch4.thetabin[which(ch4.thetabin$ch4==max(ch4.thetabin$ch4)), "Ly"]
  a0 <- max(ch4.thetabin$ch4)
  param.fit <- NA
  try(param.fit <- summary(nls(ch4 ~ a*exp(-1/2*(Ly-mu)^2/(sigma^2)),
                               weights = n,
                               start=c(mu = mu0, sigma=sigma0, a=a0),
                               dat=ch4.thetabin, algorithm = "port")))

  mu <- param.fit$coeff[1, 1]
  sigma2 <- (param.fit$coeff[2, 1])^2
  a <- param.fit$coeff[3, 1]
  ch4.thetabin$Dy <- 1/sqrt(2*pi*sigma2)*
    exp(-0.5 * ((ch4.thetabin$Ly - mu)^2/sigma2))
  ch4.thetabin$predDy <- ch4.thetabin$Dy*a*sqrt(2*pi*sigma2)

  if (plot == TRUE) {
  # Plot Gaussian Fit
    fig2 <- ggplot(ch4.thetabin, aes(x=Ly, y=ch4), col="black") +
      geom_point() +
      #geom_point(aes(y=Dy*a*sqrt(2*pi*sigma2), col="red")) +
      geom_line(aes(y=predDy), col="red") +
      theme_bw(base_size=16) +
      xlab("Ly") +
      ylab("CH4 above background")
    print(fig2)
  }
  ###############################################################################
  # Constants from Gryning et al 1983
  IDvar <- which(names(dat.filt) == "ID")
  temp <- dat.filt$temp
  pres <- dat.filt$pres
  ws3 <- dat.filt$ws3
  ws3w <- dat.filt$ws3w
  wd3_raw <- dat.filt$wd3_raw

  n <- nrow(dat.filt)
  Tbar <- mean(temp)
  Pbar <- mean(pres)
  Ubar <- mean(ws3)

  ws_sd <- sd(ws3)
  ep <- sqrt(1-(mean(sin(wd3_raw*pi/180))^2+mean(cos(wd3_raw*pi/180))^2))
  wd_sd <- asin(ep)*(1 + (2/sqrt(3) - 1)*ep^3) * 180/pi # Yamartino Method
  turbint <- sd(ws3w)/Ubar

  # PGI from turbulence
  PGturbi <- as.numeric(as.character((cut(turbint, turbint.breaks, labels=rev(seq(1,7,1))))))
  # PGI from wd sd
  PGstddevi <- as.numeric(as.character(cut(wd_sd, wdsd.breaks, labels=rev(seq(1,7,1)))))
  # Calculate average PGI, round up if 0.5
  PGI <- round((PGstddevi + PGturbi)/2 + 0.0001)
  # Merge calculated values with lag file and PSG summary file
  dist.int <- round(distance)
  pgsigmay <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                         pgsigma$PGI == PGI), "sigmay"])
  pgsigmaz <- as.numeric(pgsigma[which(pgsigma$dist.int == dist.int &
                                         pgsigma$PGI == PGI), "sigmaz"])

  # Unit conversion constants
  rt1rp1 <- 298/1013.25; gasconst <- 8.314510; mw <- 16.04;
  rt0rp0 <- (gasconst*298)/101.325; opt <- (1e-06 * mw*1000)/rt0rp0;
  # Convert a1 to g/m3
  a1_gperm3 <- a*opt*rt1rp1*Pbar/Tbar
  # Calculate PSG
  PSG <- 2*pi*a1_gperm3*Ubar*pgsigmay*pgsigmaz
  ###############################################################################
   # Check for non-representative plumes
  ch4.thetabin <- ch4.thetabin[order(ch4.thetabin$center), ]
  tails1 <- ch4.thetabin$Dy[1]*a*sqrt(2*pi*sigma2)
  tails2 <- ch4.thetabin$Dy[nrow(ch4.thetabin)]*a*sqrt(2*pi*sigma2)
  NonGauss <- FALSE
  if (tails1 > 0.1 | tails2 > 0.1) {
    NonGauss <- TRUE
  }
  return(list(ID=IDvar, PSG=PSG, a_ppm=a, Ubar = Ubar, PGI = PGI, NonGauss = NonGauss,PG.s=PGstddevi,PG.t=PGturbi))
}

