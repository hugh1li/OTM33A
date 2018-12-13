
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
  bg <- attr(dat,"CH4.bg")
  dat.filt = dat[sub==TRUE & theta.filter==TRUE]
  dat.summary <- c(
    mn.WD.2D = dat.filt[,mean(wd2)],
    mn.WD.3D = dat.filt[,mean(wd3)],
    sd.WD.2D = dat.filt[,sd.wd.yam(wd2)],
    sd.WD.3D = dat.filt[,sd.wd.yam(wd3)],
    pressure = dat.filt[,mean(Pressure)],
    u = dat.filt[,mean(X3DS.U)],
    v = dat.filt[,mean(X3DS.V)],
    w = dat.filt[,mean(X3DS.W)],
    temp = dat.filt[,mean(Temp)],
    u.u = dat.filt[,mean(X3DS.U^2)],
    v.v = dat.filt[,mean(X3DS.V^2)],
    w.w = dat.filt[,mean(X3DS.W^2)],
    temp.temp = dat.filt[,mean(Temp^2)],
    u.v = dat.filt[,mean(X3DS.U*X3DS.V)],
    u.w = dat.filt[,mean(X3DS.U*X3DS.W)],
    u.t = dat.filt[,mean(X3DS.U*Temp)],
    v.w = dat.filt[,mean(X3DS.V*X3DS.W)],
    v.t = dat.filt[,mean(X3DS.V*Temp)],
    w.t = dat.filt[,mean(X3DS.W*Temp)],
    gps.lat.mn = dat.filt[,mean(GPS.Latitude)],
    gps.lat.sd = dat.filt[,sd(GPS.Latitude)],
    gps.lon.mn = dat.filt[,mean(GPS.Longitude)],
    gps.lon.sd = dat.filt[,sd(GPS.Longitude)],
    CO2.mn = dat.filt[,mean(CO2)],
    CO2.sd = dat.filt[,sd(CO2)],
    N.5.percent = round(dat.filt[,.N]*0.05),
    CH4.max = dat.filt[,max(CH4)] + bg,
    CH4.mn.95 = dat.filt[CH4>quantile(CH4,.95),mean(CH4)] + bg,
    CH4.sd.95 = dat.filt[CH4>quantile(CH4,.95),sd(CH4)],
    CH4.min = dat.filt[,min(CH4)] + bg,
    CH4.mn.5 = dat.filt[CH4<quantile(CH4,.05),mean(CH4)] + bg,
    CH4.sd.5 = dat.filt[CH4<quantile(CH4,.05),sd(CH4)],
    CH4.mn = dat.filt[,mean(CH4)]+bg,
    ws3.mn = dat.filt[,mean(ws3)],
    temp.K = dat.filt[,mean(Temp)]+273.15,
    wd2.mn = dat.filt[,mean(wd2)],
    wd3.mn = dat.filt[,mean(wd3)],
    wd2.sd = dat.filt[,sd(wd2)],
    wd3.sd = dat.filt[,sd(wd3)],
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
    a1 = attr(dat,"CH4.Ly.peak"),
    sigma = attr(dat,"CH4.Ly.sigma"),
    CH4.a1 = attr(dat,"CH4.Ly.peak.g.per.m3"),
    QA.bin = (as.numeric(attr(dat,"CH4.wd.rot"))+180)/10, # I can't discern from the Matlab code what this value is supposed to be
    QA.count = dat.filt[,.N,thetabin][,max(N)],
    temp.ratio = dat.filt[,(mean(Temp)+273.15)/(mean(Temp.Alt)+273.15)],
    temp.sd = dat.filt[,sd(Temp)],
    temp.min = dat.filt[,min(Temp)],
    temp.max = dat.filt[,max(Temp)],
    PSG = attr(dat,"CH4.PSG"),
    distance = attr(dat,"distance")
  )
  return(dat.summary)
}
