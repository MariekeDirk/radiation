#' R2
#'
#' 
#' 
#' @author Marieke Dirksen, \email marieke.dirksen@knmi.nl
#' @return RMSE
#' @export
#'
#'
r2<-function(residual,observed){
  teller <- sum(residual^2)
  noemer <- sum((observed-mean(observed))^2)
  r2 <- 1 - teller/noemer
  return(r2)
}

