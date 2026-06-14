calculate_nf <- function(Amat) {
  A <- Amat[1,1]
  B <- Amat[1,2]
  C <- Amat[2,1]
  D <- Amat[2,2]
  
  rho <- exp((A+D-B-C)/2)
  fd <- exp((C+D-A-B)/2)
  
  data.frame(
    nd=1-rho,
    fd=fd
  )
}
