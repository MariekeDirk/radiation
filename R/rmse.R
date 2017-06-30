#' RMSE
#'
#' 
#' 
#' @author Marieke Dirksen, \email marieke.dirksen@knmi.nl
#' @return RMSE
#' @export
#'
#'
rmse<-function(sim,obs){
  rmse=sqrt(mean((sim-obs)^2,na.rm=TRUE))
  }
