calculate_nf_stoch <- function(Amat,
                               k) {
  A <- Amat[1,1]
  B <- Amat[1,2]
  C <- Amat[2,1]
  D <- Amat[2,2]
  
  sigma_a <- k*A
  sigma_b <- k*B
  sigma_c <- k*C
  sigma_d <- k*D
  
  rho <- exp((A+D-B-C)/2-((sigma_a-sigma_c)^2+(sigma_b-sigma_d)^2)/4)
  fd <- exp((C+D-A-B)/2+((sigma_a-sigma_c)^2-(sigma_b-sigma_d)^2)/4)
  
  data.frame(
    rho=rho,
    nd=1-rho,
    fd=fd
  )
}
