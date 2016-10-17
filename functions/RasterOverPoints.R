#' Raster Over Points
#'
#' 
#' 
#' @author Marieke Dirksen, \email marieke.dirksen@knmi.nl
#' @return Returns at the location of the observations: observations, layer values and difference
#' @export
#'
#'
RasterOverPoints<-function(stacklayer,obspoints,variable,pro){
  ASC<-asc.from.raster(stacklayer)
  spdf<-asc2spixdf(ASC)
  proj4string(spdf)<-pro
  
  var<-over(obspoints,spdf)
  
  n<-names(var)
  diff<-var[n]-variable
  
  observed<-data.frame(variable)
  layer<-var[n]
  
  #out<-diff
  out<-data.frame(observed,layer,diff)
  colnames(out)<-c("observed","layer","difference")
  return(out)
}
