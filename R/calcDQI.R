# DQI values are documented in Appendix D to Appendix F1 of OTM 33A, v 1.3, p 53 to 59
calcDQI <- function(dat) {
  dat.summary <- list(
  dat.filt = dat[sub==TRUE & theta.filter==TRUE],
  mn.WD.2D = dat.filt[,mean(WS.Wind.Direction)],
  mn.WD.3D = dat.filt[,mean(X3DS.Azimuth)],
  sd.WD.2D = dat.filt[,sd.wd3.yam(WS.Wind.Direction)],
  sd.WD.3D = dat.filt[,sd.wd3.yam(X3DS.Azimuth)],
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
  CH4.mn = dat.filt[,mean(CH4)],
  ws3.mn = dat.filt[,mean(ws3)],
  temp.K = dat.filt[,mean(X3DS.Sonic.Temp)]+273.15,
  ws2.mn = dat.filt[,mean(WS.Wind.Speed)],
  wd3.mn = dat.filt[,mean(wd3)],
  ws2.sd = dat.filt[,sd(WS.Wind.Speed)],
  wd3.sd = dat.filt[,sd(wd3)],
  wd3.sd = dat.filt[,sd.wd3.yam(X3DS.Aziumth)],
  wd2.sd = dat.filt[,sd.wd3.yam(WS.Wind.Direction)],
  turbint = attr(dat,"turbint"),
  PG.sd.2 = attr(dat,"PG.sd.2"),
  PG.sd.3 = attr(dat,"PG.sd.3"),
  PG.ti = attr(dat,"PG.ti"),
  pgsigmay = attr(dat,"pgsigmay"),
  pgsigmaz = attr(dat,"pgsigmaz"),
  centroid.dir = NA,
  binwidth = attr(dat,"binwidth")
  )
  dat.attr <- list(
    Filename = attr(dat,"file.name"),
    Event = NA,
    Distance = attr(dat,"distance"),
    Sensor.Height = NA,
    Source.Height = NA,
    a1.plus.bkgCH4 = attr(dat,"Ly.peak")+attr(dat,"CH4.bg"),
    bkgCH4 =attr(dat,"CH4.bg"),
    Pressure = pressure,
    #Count.DQI.1, IF(AG3<=25,3,""), 5Percent.N
    Count.DQI.1 = round(dat[,.N])*.05,
    # Count.DQI.2, IF(BQ3>=3*AG3,"",3), Count.QA >= 3*5%.N
    # No formula for Count.QA available - not calculated
    # Wind.DQI.1,	IF(AVERAGE(AT3:AU3) >30,1,""), mean(Std.WD.2D,Std.WD.3D)>30
    Wind.DQI.1 = mean(sd.WD.2D,sd.WD.3D),
    #Wind.DQI.2,	IF(ABS((AT3-AU3)/AVERAGE(AT3:AU3))*100<50,"",1), abs(Std.WD.2D-Std.WD.3D)/mean(Std.WD.2D,Std.WD.3D)*100<50
    Wind.DQI.2 = abs(sd.WD.2D-sd.WD.3D)/mean(sd.WD.2D,sd.WD.3D),
    #Turb.WS.Int.DQI.1, IF(AND(AV3>0.22,CW3=""),3,""), Turbulent.Intensity>.22
    Turb.WS.Int.DQI.1 = attr(dat,"turbint"),
    #WS.DQI.1,	IF(AP3<1.5,1,""), 3DS.2D.Wind.Speed < 1.5
#WS.DQI.2, IF(AP3<1,5,""), 3DS.2D.Wind.Speed < 1.0
#WS.Var.DQI.1, IF(CH3<=2.5,"",5), Wind.Var.DQI <= 2.5
#WS.Var.DQI.2, IF(CH3<=5,"",10), Wind.Var.DQI <= 5.0
#Bin.DQI.1, IF(AND(BP3>=15, BP3<=21),"",1), BinQA>=15 & BinQA<=21
#Bin.DQI.2, IF(AND(BP3>=13, BP3<=23),"",3), BinQA>=13 & BinQA<=23
#Fit.DQI.1, IF(BO3>=0.95,"",5), rsq>=.95
#Fit.DQI.2, IF(BO3>=0.9,"",10), rsq>=.9
#CH4.High.DQI.1, IF(AH3<=100,"",1), High.Methane<=100
#CH4.High.DQI.2, IF(AI3<(2*AJ3),1,""), Avg.High.5Percent.Methane<2*Stddev.High.5Percent.Methane
#CH4.High.DQI.3, IF(AI3<(1*AJ3),5,""), Avg.High.5Percent.Methane<Stddev.High.5Percent.Methane
#CH4.High.DQI.4, IF(AND(CT3=1, CV3=1, DF3=3),5,"")
#CH4.Low.DQI.1, IF(AH3<=1.7,1,""), High.Methane<=1.7
#CH4.Low.DQI.2, IF(AN3<=((10*AM3)+AK3),1,""), Mean.Methane<= 10*Stddev.Low.5Percent.Methane + Low.Methane
#CH4.Low.DQI.3, IF(BK3<=0.15, IF(G3<=50,10,IF(AND(G3>50,G3<=100),5,IF(AND(G3>100,G3<=150),3,IF(G3>150,1)))),""), a1 & distance
#CH4.Low.DQI.4, IF(BK3<=0.1, IF(G3<=50,10,IF(AND(G3>50,G3<=100),5,IF(AND(G3>100,G3<=150),3,IF(G3>150,1)))),""), a1 & distance
#Sig.y.DQI, IF((BD3/BN3)>=1.2,1,""), PG.sigma.y/sigma.y >= 1.2
#PSG.bLs.High.DQI, IF(BY3/CG3>=2,1,""), PSG.Emission.Estimate/A1.bLs >= 2.1
#PSG/bLs.Low.DQI, IF(BY3/CG3<=0.5,1,""), PSG.Emission.Estimate/A1.bLs < 0.5
#PSG.Sigy.High.DQI, IF(BY3/BZ3>=2,1,""), PSG.Emission.Estimate/Sig.y.Emission.Estimate > 2
#PSG.Sigy.Low.DQI, IF(BY3/BZ3<=0.5,1,""), PSG.Emission.Estimate/Sig.y.Emission.Estimate < 0.5
#Dist.DQI, IF(AND(G4>=25, G4<=150),"",1), Distance >= 25 & Distance < 150
#GPS.DQI, IF(AND(AB3<=0.000002, AD3<=0.000002),"",1), GPS.Latitude.StdDev < 0.000002 & GPS.Longitude.StdDev < 0.000002
#CO2.Level.DQI, IF(AND(AE3>=360, AE3<=410),"",1), Mean.CO2 > 360 & Mean.CO2 < 410
#CO2.Variance.DQI, IF(AF3<=10,"",1), Stdev.CO2 < 10
#Temp.Comp.DQI, IF(ABS((1-BR3)*100)<=1,"",1), abs(1-Temp.QA)*100 < 1
#3Dtemp.StdDev.DQI, IF(BS3<=1.1,"",1), 3Dtemp.stdev.QA < 1.1
  )
  return(list(dat.summary,dat.attr))
}
