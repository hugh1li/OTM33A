
#' Caculate DQI statistics at the end of the batch.
#' DQI statistics are documented in Appendix D to Appendix F1 of OTM 33A, v 1.3, p 53 to 59
#'
#' @param
#' dat:  Data table, after running emissions estimate calcs
#'
#' @return list of statistics used to calculate DQIs
#' @export
#' @examples
#' y = evalDQI(dat)
calcDQI <- function(dat) {
  dat.filt = dat[sub==TRUE & theta.filter==TRUE]
  dat.summary <- c(
    mn.WD.2D = dat.filt[,mean(WS.Wind.Direction)],
    mn.WD.3D = dat.filt[,mean(X3DS.Azimuth)],
    sd.WD.2D = dat.filt[,sd.wd.yam(WS.Wind.Direction)],
    sd.WD.3D = dat.filt[,sd.wd.yam(X3DS.Azimuth)],
    pressure = dat.filt[,mean(WS.Bar..Pressure)],
    u = dat.filt[,mean(X3DS.U)],
    v = dat.filt[,mean(X3DS.V)],
    w = dat.filt[,mean(X3DS.W)],
    temp = dat.filt[,mean(X3DS.Sonic.Temp)],
    u.u = dat.filt[,mean(X3DS.U^2)],
    v.v = dat.filt[,mean(X3DS.V^2)],
    w.w = dat.filt[,mean(X3DS.W^2)],
    temp.temp = dat.filt[,mean(X3DS.Sonic.Temp^2)],
    u.v = dat.filt[,mean(X3DS.U*X3DS.V)],
    u.w = dat.filt[,mean(X3DS.U*X3DS.W)],
    u.t = dat.filt[,mean(X3DS.U*X3DS.Sonic.Temp)],
    v.w = dat.filt[,mean(X3DS.V*X3DS.W)],
    v.t = dat.filt[,mean(X3DS.V*X3DS.Sonic.Temp)],
    w.t = dat.filt[,mean(X3DS.W*X3DS.Sonic.Temp)],
    gps.lat.mn = dat.filt[,mean(GPS.Latitude)],
    gps.lat.sd = dat.filt[,sd(GPS.Latitude)],
    gps.lon.mn = dat.filt[,mean(GPS.Longitude)],
    gps.lon.sd = dat.filt[,sd(GPS.Longitude)],
    CO2.mn = dat.filt[,mean(CO2)],
    CO2.sd = dat.filt[,sd(CO2)],
    N.5.percent = round(dat.filt[,.N]*0.05),
    CH4.max = dat.filt[,max(CH4.nbg)],
    CH4.mn.95 = dat.filt[CH4.nbg>quantile(CH4.nbg,.95),mean(CH4.nbg)],
    CH4.sd.95 = dat.filt[CH4.nbg>quantile(CH4.nbg,.95),sd(CH4.nbg)],
    CH4.min = dat.filt[,min(CH4.nbg)],
    CH4.mn.5 = dat.filt[CH4.nbg<quantile(CH4.nbg,.05),mean(CH4.nbg)],
    CH4.sd.5 = dat.filt[CH4.nbg<quantile(CH4.nbg,.05),sd(CH4.nbg)],
    CH4.mn.raw = dat.filt[,mean(CH4.nbg)],
    CH4.mn = dat.filt[,mean(CH4.nbg)],
    ws3.mn = dat.filt[,mean(ws3)],
    temp.K = dat.filt[,mean(X3DS.Sonic.Temp)]+273.15,
    wd2.mn = dat.filt[,mean(WS.Wind.Direction)],
    wd3.mn = dat.filt[,mean(X3DS.Azimuth)],
    wd2.sd = dat.filt[,sd(WS.Wind.Direction)],
    wd3.sd = dat.filt[,sd(X3DS.Azimuth)],
    turbint = attr(dat,"turbint"),
    PG.sd.2 = attr(dat,"PG.sd.2"),
    PG.sd.3 = attr(dat,"PG.sd.3"),
    PG.ti = attr(dat,"PG.ti"),
    pgsigmay = attr(dat,"pgsigmay"),
    pgsigmaz = attr(dat,"pgsigmaz"),
    centroid.dir = NA,
    binwidth = attr(dat,"binwidth"),
    cut.wind.speed = NA,
    cut.bin.density = attr(dat,"min.n"),
    a1 = attr(dat,"Ly.peak"),
    sigma = attr(dat,"Ly.sigma"),
    CH4.a1 = attr(dat,"Ly.peak.g.per.m3"),
    QA.bin = 18, # I can't discern from the Matlab code what this value is supposed to be
    QA.count = dat.filt[,.N,thetabin][,max(N)],
    temp.ratio = dat.filt[,(mean(WS.Temperature)+273.15)/(mean(X3DS.Sonic.Temp)+273.15)],
    temp.sd = dat.filt[,sd(X3DS.Sonic.Temp)],
    temp.min = dat.filt[,min(X3DS.Sonic.Temp)],
    temp.max = dat.filt[,max(X3DS.Sonic.Temp)],
    PSG = attr(dat,"PSG")
  )
  return(dat.summary)
}
