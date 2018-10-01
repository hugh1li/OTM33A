###############################################################################
# Function to import STR files, adjust lag (of CH4) and rotate wind coordinates
# Halley Brantley
# 9/28/15
###############################################################################

importSTR <- function(STRfile, lagfile, ID) {
  # Inputs
  # STRfile - path to STRfile to import
  # lagfile - path to file containing lag times and distances
  # ID - identification used to map STRfile to lagfile 
  # Outputs
  # dat - lag adjusted
  # sonic wind speeds are rotated so mean(ws3u)=mean(ws3v)=0, 
  # if mean(ws3v)>0, wd3 centered at 0 and ranges from -180 to 180
  # if mean(ws3v)<0, wd3 centered at 180 and ranges from 0 to 360
  library(readxl)
  lags <- read.csv(lagfile, header=TRUE)
  names(lags)[3:6] <- c("distance", "sensor_ht", "source_ht", "release_rate")
  lagT <- lags[which(lags$ID==ID),"lag"]
  runinfo <- lags[which(lags$ID==ID),]
  
  ftype <- substring(STRfile, nchar(STRfile)-3, nchar(STRfile))
  if (ftype == ".xls") {
    rawdat <- data.frame(read_excel(STRfile))
    rawdat$time <- as.POSIXct(strptime(paste(substr(ID, 2, 7), substr(rawdat$Time, 12, 20)), 
                                       format = "%m%d%y %H:%M:%S"))    
  } else {
    if(ID %in% c("9060513_01", "9060513_02", "9060513_03", "9060513_04", "9060513_05",
                 "9071112_01", "9071112_02", "9071112_03", "9071112_05", "9071112_06",
                 "9071112_12")){
      numskip <-  8
    } else {
      numskip <- 7
    }
    rawdat <- read.csv(STRfile, skip=numskip)
    rawdat$time <- as.POSIXct(strptime(rawdat[, 1], 
                                       format = "%Y-%m-%dT%H:%M:%S+00:00"))
  }
  # Only keep rows with a timestamp (removes extra rows at end of file)
  rawdat <- subset(rawdat, !is.na(rawdat$time))
  # Rename columns to match
  names(rawdat)[grep("CH4", names(rawdat))] <- "CH4_raw"
  names(rawdat)[grep("Wind.Speed", names(rawdat))] <- "WS.Wind.Speed"
  names(rawdat)[grep("Bar", names(rawdat))] <- "WS.Pressure"
  rawdat$temp <- rawdat$X3DS.Sonic.Temp + 273.15
  # Add lag to CH4
  dat <- rawdat[1:(nrow(rawdat)-lagT), c("time", "GPS.Latitude", "GPS.Longitude", 
                                        "WS.Wind.Speed", "WS.Wind.Direction",
                                        "temp", "WS.Pressure", "X3DS.U", 
                                        "X3DS.V", "X3DS.W", "X3DS.Azimuth", 
                                        "X3DS.Sonic.Temp", "CH4_raw")]
  names(dat) <- c("time", "lat", "lon",  "ws2_raw", "wd2_raw", "temp", "pres", 
                  "ws3u_raw", "ws3v_raw", "ws3w_raw",
                  "wd3_raw",   "ws3t", "CH4_raw")
  dat[1:nrow(dat),  "ch4"] <- rawdat[(lagT+1):nrow(rawdat), "CH4_raw"]
  if(ID %in% c("9060512_01", "9060512_02", "9060512_03", "9060512_04", "9060512_05",
               "9060512_06", "9060512_07", "9060512_08", "9060512_09", "9060512_10",
               "9060512_11", "9060512_12")) {
    dat$ch4 <- dat$ch4/1.0671 #Calibration correction, NEIC  
  }
  #############################################################################
  # 2 rotations to streamline coordinates
  RA <-  atan2(-mean(dat$ws3u_raw),-mean(dat$ws3v_raw)) + pi 
  ws3v_rot1 <- dat$ws3v_raw*cos(RA) + dat$ws3u_raw*sin(RA)
  dat$ws3u <- (-1)*dat$ws3v_raw*sin(RA) + dat$ws3u_raw*cos(RA)
  RB <-  atan2(-mean(dat$ws3w_raw), -mean(ws3v_rot1)) + pi
  dat$ws3v <- ws3v_rot1*cos(RB) + dat$ws3w_raw*sin(RB)
  dat$ws3w <- (-1)*ws3v_rot1*sin(RB) + dat$ws3w_raw*cos(RB)
  # Calculate new wind direction
  dat$wd3 <- atan2(dat$ws3u,dat$ws3v)*180/pi
  dat$ws3 <- sqrt(dat$ws3u^2+dat$ws3v^2)
  dat$ID <- ID
  # subtract background concentrations
  #ch4_bg <- mean(dat$ch4[which(dat$ch4 < quantile(dat$ch4, 0.05))])
  ch4_bg <- mean(dat$ch4[order(dat$ch4)][1:nrow(dat)*0.05]) 
  dat$ch4L <- dat$ch4-ch4_bg
  #############################################################################
  # Center wind coordinates so mean is 0
  dat$ws3vprime <- dat$ws3v - mean(dat$ws3v)
  dat$ws3tprime <- dat$ws3t - mean(dat$ws3t)
  #############################################################################
  if(mean(dat$ws3v) < 0){
    dat$wd3 <- ifelse(dat$wd3 < 0, dat$wd3+360, dat$wd3)
  }
  return(list(dat = dat, runinfo = runinfo))
}


