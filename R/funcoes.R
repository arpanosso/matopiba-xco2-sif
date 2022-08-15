
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}


r2findWLS <-function(fit, vario){
  SSErr<-attr(fit,"SSErr")
  weig<-vario$np/vario$dist^2
  SStot<- SStot <- sum((weig*(vario$gamma-mean(vario$gamma)))^2)
  R2<-1-SSErr/SStot
  return(R2)
}
