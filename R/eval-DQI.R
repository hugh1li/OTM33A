#' Caculate DQI values from post-emissions-calc data set and DQI statistics from calcDQI.
#' DQI values are documented in Appendix D to Appendix F1 of OTM 33A, v 1.3, p 53 to 59
#'
#' @param
#' dat:  Data table, after running emissions estimate calcs
#' stats:  Output from calcDQI
#'
#' @return list of statistics used to calculate DQIs
#' @export
#' @examples
#' y = evalDQI(dat)


evalDQI <- function(x,stats) {
  with(stats, dat.attr <- c(
  #Count.DQI.1, IF(AG3<=25,3,""), 5Percent.N
  Count.DQI.1 = ifelse(N.5.percent<=25,3,0),
  # Count.DQI.2, IF(BQ3>=3*AG3,"",3), Count.QA >= 3*5%.N
  Count.DQI.2 = ifelse(QA.count >= 3*N.5.percent,0,3),
  # Wind.DQI.1,	IF(AVERAGE(AT3:AU3) >30,1,""), mean(Std.WD.2D,Std.WD.3D)>30
  Wind.DQI.1 = ifelse(mean(sd.WD.2D,sd.WD.3D)>30,1,0),
  #Wind.DQI.2,	IF(ABS((AT3-AU3)/AVERAGE(AT3:AU3))*100<50,"",1), abs(Std.WD.2D-Std.WD.3D)/mean(Std.WD.2D,Std.WD.3D)*100<50
  Wind.DQI.2 = ifelse(abs(sd.WD.2D-sd.WD.3D)/mean(sd.WD.2D,sd.WD.3D)<.5,0,1),
  #Turb.WS.Int.DQI.1, IF(AND(AV3>0.22,CW3=""),3,""), Turbulent.Intensity>.22
  Turb.WS.Int.DQI.1 = ifelse(turbint>.22 & ws3.mn > 1.5, 3,0),
  #WS.DQI.1,	IF(AP3<1.5,1,""), 3DS.2D.Wind.Speed < 1.5
  WS.DQI.1 = ifelse(ws3.mn<1.5,1,0),
  #WS.DQI.2, IF(AP3<1,5,""), 3DS.2D.Wind.Speed < 1.0
  WS.DQI.2 = ifelse(ws3.mn<1,5,0),
  #WS.Var.DQI.1, IF(CH3<=2.5,"",5), Wind.Var.DQI <= 2.5
  #WS.Var.DQI.2, IF(CH3<=5,"",10), Wind.Var.DQI <= 5.0
  #Bin.DQI.1, IF(AND(BP3>=15, BP3<=21),"",1), BinQA>=15 & BinQA<=21
  Bin.DQI.1 = ifelse(QA.bin >= 15 & QA.bin <= 21,0,1),
  #Bin.DQI.2, IF(AND(BP3>=13, BP3<=23),"",3), BinQA>=13 & BinQA<=23
  Bin.DQI.2 = ifelse(QA.bin >= 12 & QA.bin <= 24,0,3),
  #Fit.DQI.1, IF(BO3>=0.95,"",5), rsq>=.95
  #Fit.DQI.2, IF(BO3>=0.9,"",10), rsq>=.9
  #CH4.High.DQI.1, IF(AH3<=100,"",1), High.Methane<=100
  CH4.High.DQI.1 = ifelse(CH4.max <= 100, 0, 1),
  #CH4.High.DQI.2, IF(AI3<(2*AJ3),1,""), Avg.High.5Percent.Methane<2*Stddev.High.5Percent.Methane
  CH4.High.DQI.2 = ifelse(CH4.mn.95 <= 2*CH4.sd.95, 1, 0),
  #CH4.High.DQI.3, IF(AI3<(1*AJ3),5,""), Avg.High.5Percent.Methane<Stddev.High.5Percent.Methane
  CH4.High.DQI.3 = ifelse(CH4.mn.95 <= CH4.sd.95, 5, 0),
  #CH4.High.DQI.4, IF(AND(CT3=1, CV3=1, DF3=3),5,"")
  CH4.High.DQI.4 = ifelse(CH4.mn.95 <= CH4.sd.95, 5, 0),
  #CH4.Low.DQI.1, IF(AH3<=1.7,1,""), High.Methane<=1.7
  CH4.Low.DQI.1 = ifelse(CH4.max <= 1.7, 1,0),
  #CH4.Low.DQI.2, IF(AN3<=((10*AM3)+AK3),1,""), Mean.Methane<= 10*Stddev.Low.5Percent.Methane + Low.Methane
  CH4.Low.DQI.2 = ifelse(CH4.mn <= 10*CH4.sd.5+CH4.min,1,0),
  #CH4.Low.DQI.3, IF(BK3<=0.15, IF(G3<=50,10,IF(AND(G3>50,G3<=100),5,IF(AND(G3>100,G3<=150),3,IF(G3>150,1)))),""), a1 & distance
  CH4.Low.DQI.3 = ifelse(a1 <= .15, if( distance <=50) 10 else if(abs(distance-75) < 25) 5 else if(abs(distance-125) < 25) 3 else 1, 0),
  #CH4.Low.DQI.4, IF(BK3<=0.1, IF(G3<=50,10,IF(AND(G3>50,G3<=100),5,IF(AND(G3>100,G3<=150),3,IF(G3>150,1)))),""), a1 & distance
  CH4.Low.DQI.4 = ifelse(a1 <= .1, if( distance <=50) 10 else if(abs(distance-75) < 25) 5 else if(abs(distance-125) < 25) 3 else 1, 0),
  #Sig.y.DQI, IF((BD3/BN3)>=1.2,1,""), PG.sigma.y/sigma.y >= 1.2
  Sig.y.DQI = ifelse(pgsigmay/sigma > 1.2, 1, 0),
  #PSG.bLs.High.DQI, IF(BY3/CG3>=2,1,""), PSG.Emission.Estimate/A1.bLs >= 2.1
  #PSG.bLs.Low.DQI, IF(BY3/CG3<=0.5,1,""), PSG.Emission.Estimate/A1.bLs < 0.5
  #PSG.Sigy.High.DQI, IF(BY3/BZ3>=2,1,""), PSG.Emission.Estimate/Sig.y.Emission.Estimate > 2
  PSG.Sigy.High.DQI = ifelse(2*pgsigmay*pgsigmaz/sigma^2>2,1,0),
  #PSG.Sigy.Low.DQI, IF(BY3/BZ3<=0.5,1,""), PSG.Emission.Estimate/Sig.y.Emission.Estimate < 0.5
  PSG.Sigy.Low.DQI = ifelse(2*pgsigmay*pgsigmaz/sigma^2<0.5,1,0),
  #Dist.DQI, IF(AND(G4>=25, G4<=150),"",1), Distance >= 25 & Distance < 150
  Dist.DQI = ifelse(distance >= 25 & distance < 150, 0, 1),
  #GPS.DQI, IF(AND(AB3<=0.000002, AD3<=0.000002),"",1), GPS.Latitude.StdDev < 0.000002 & GPS.Longitude.StdDev < 0.000002
  GPS.DQI = ifelse(gps.lat.sd < 0.000002 & gps.lon.sd < 0.000002, 0, 1),
  #CO2.Level.DQI, IF(AND(AE3>=360, AE3<=410),"",1), Mean.CO2 > 360 & Mean.CO2 < 410
  CO2.Level.DQI = ifelse(abs(CO2.mn-385) < 25,0,1),
  #CO2.Variance.DQI, IF(AF3<=10,"",1), Stdev.CO2 < 10
  CO2.Variance.DQI = ifelse(CO2.sd <= 10,0,1),
  #Temp.Comp.DQI, IF(ABS((1-BR3)*100)<=1,"",1), abs(1-Temp.QA)*100 < 1
  Temp.Comp.DQI = ifelse(abs(1-temp.ratio) < 0.01, 0, 1),
  #3Dtemp.StdDev.DQI, IF(BS3<=1.1,"",1), 3Dtemp.stdev.QA < 1.1
  Temp3D.StdDev.DQI = ifelse(temp.sd<1.1,0,1)
  )
  )
}
