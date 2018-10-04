#' Calculate wind direction and caclulate wd3, the rotated wind direction variable
#' @param dat Data table, with names obtained from GMAP data output as of 2018-Aug
#' @return void Columns are added to dat data table
#' @export
#' @examples
#' rotateWindDirection(dat)

rotateWindDirection <- function(dat) {
  if(is.null(attr(dat,"rotation.a"))) {
    # Relies upon pass-by-reference semantics of data.table
    # Calculate first rotation angle
    RA=dat[,atan2(-mean(X3DS.U),-mean(X3DS.V))+pi]
    # Apply first rotation - U axis
    dat[,ws3u := X3DS.U*cos(RA) - X3DS.V*sin(RA) ]
    # Apply first rotation - V axis
    dat[,ws3v_rot1 := X3DS.U*sin(RA) + X3DS.V*cos(RA)]
    # Calculate second rotation angle
    RB <-  dat[,atan2(-mean(X3DS.W), -mean(ws3v_rot1)) + pi]
    # Apply second rotation - V axis
    dat[,ws3v := ws3v_rot1*cos(RB) + X3DS.W*sin(RB)]
    # Apply second rotation - W axis
    dat[,ws3w := X3DS.W*cos(RB) - ws3v_rot1*sin(RB)]
    # Calculate new wind direction & speed
    dat[,wd3 := atan2(ws3u,ws3v)*180/pi]
    dat[,ws3 := sqrt(ws3u^2+ws3v^2)]
    if(dat[,mean(ws3v)] < 0){
      dat[,wd3 := ifelse(wd3 < 0, wd3+360, wd3)]
    }
    setattr(dat,"rotation.a",RA)
    setattr(dat,"rotation.b",RB)
  }
  return(dat)
}
