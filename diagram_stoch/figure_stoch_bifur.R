library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(egg)
library(tikzDevice)

gfun <- function(x, amat, k) {
  R <- amat[1,1]
  S <- amat[1,2]
  T <- amat[2,1]
  P <- amat[2,2]
  
  xstar <- c(P-S)/(R+P-T-S)
  K <- 1/(k^2 * (R+P-T-S))
  
  if (k > 0) {
    k^2 * (R+P-T-S)^2 * (x-xstar) * (K + (1/2-x) * (x-xstar))
  } else {
    R * x + S * (1-x) - T * x - P * (1-x)
  }
}

dfdx <- function(x, amat, k) {
  R <- amat[1,1]
  S <- amat[1,2]
  T <- amat[2,1]
  P <- amat[2,2]
  
  xstar <- c(P-S)/(R+P-T-S)
  K <- 1/(k^2 * (R+P-T-S))
  
  if (k > 0) {
    dGdx <- k^2 * (R+P-T-S)^2 * (K + (1/2-x) * (x-xstar)) +
      k^2 * (R+P-T-S)^2 * (x-xstar) * (
        (1/2+xstar) - 2 * x
      )
  } else {
    dGdx <- R+P-T-S
  }
  
  x*(1-x) * dGdx + (1-2*x) * gfun(x, amat, k)
}

eqfun <- function(amat, kvec) {
  lapply(kvec, function(k) {
    eq1 <- 0
    eq2 <- 1
    
    st1 <- dfdx(eq1, amat, k) < 0
    st2 <- dfdx(eq2, amat, k) < 0
    
    R <- amat[1,1]
    S <- amat[1,2]
    T <- amat[2,1]
    P <- amat[2,2]
    
    xstar <- c(P-S)/(R+P-T-S)
    
    if(0 < xstar & xstar < 1) {
      eq3 <- xstar
      st3 <- dfdx(eq3, amat, k) < 0
    } else {
      eq3 <- NA
      st3 <- NA
    }
    
    K <- 1/(k^2 * (R+P-T-S))
    
    if ((1/2-xstar)^2 + 4 * K > 0) {
      eq4 <- (xstar + 1/2 + sqrt((1/2-xstar)^2 + 4 * K))/2
      eq5 <- (xstar + 1/2 - sqrt((1/2-xstar)^2 + 4 * K))/2
      st4 <- dfdx(eq4, amat, k) < 0
      st5 <- dfdx(eq5, amat, k) < 0
    } else {
      eq4 <- NA
      eq5 <- NA
      st4 <- NA
      st5 <- NA
    }
    
    data.frame(
      k=k,
      eq=c(eq1, eq2, eq3, eq4, eq5),
      st=c("Unstable", "Stable")[c(st1, st2, st3, st4, st5)+1],
      group=1:5
    )
  }) %>%
    bind_rows
}

amat_pd <- matrix(c(0.4, -0.4, 1, 0), 2, 2, byrow=TRUE)
amat_hg <- matrix(c(1, 0.4, 0.4, 0), 2, 2, byrow=TRUE)
amat_co <- matrix(c(1, -0.4, 0.4, 0), 2, 2, byrow=TRUE)
amat_sd <- matrix(c(0.4, 0.4, 1, 0), 2, 2, byrow=TRUE)

kvec <- seq(0, 4, length.out=201)

eq_pd <- eqfun(amat_pd, kvec) %>%
  filter(!is.na(eq), eq >= 0, eq <= 1)

eq_hg <- eqfun(amat_hg, kvec) %>%
  filter(!is.na(eq), eq >= 0, eq <= 1)

eq_co <- eqfun(amat_co, kvec) %>%
  filter(!is.na(eq), eq >= 0, eq <= 1)

eq_sd <- eqfun(amat_sd, kvec) %>%
  filter(!is.na(eq), eq >= 0, eq <= 1)

g1 <- ggplot(eq_pd) +
  geom_path(aes(k, eq, group=interaction(group, st), linetype=st), lwd=1) +
  annotate("text", x=3.8, y=1, label="PD", hjust=1, vjust=-0.3) +
  scale_x_continuous("Amount of stochasticity, $k$", expand=c(0, 0)) +
  scale_y_continuous("Cooperation frequency", limits=c(-0.2, 1.2), expand=c(0, 0),
                     breaks=c(0, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "top"
  )

g2 <- ggplot(eq_hg) +
  geom_path(aes(k, eq, group=interaction(group, st), linetype=st), lwd=1) +
  annotate("text", x=3.8, y=1, label="HG", hjust=1, vjust=-0.3) +
  scale_x_continuous("Amount of stochasticity, $k$", expand=c(0, 0)) +
  scale_y_continuous("Cooperation frequency", limits=c(-0.2, 1.2), expand=c(0, 0),
                     breaks=c(0, 1))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(eq_co) +
  geom_path(aes(k, eq, group=interaction(group, st), linetype=st), lwd=1) +
  annotate("text", x=3.8, y=1, label="SH", hjust=1, vjust=-0.3) +
  scale_x_continuous("Amount of stochasticity, $k$", expand=c(0, 0)) +
  scale_y_continuous("Cooperation frequency", limits=c(-0.2, 1.2), expand=c(0, 0),
                     breaks=c(0, 1))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "none"
  )

g4 <- ggplot(eq_sd) +
  geom_path(aes(k, eq, group=interaction(group, st), linetype=st), lwd=1) +
  annotate("text", x=3.8, y=1, label="HD", hjust=1, vjust=-0.3) +
  scale_x_continuous("Amount of stochasticity, $k$", expand=c(0, 0)) +
  scale_y_continuous("Cooperation frequency", limits=c(-0.2, 1.2), expand=c(0, 0),
                     breaks=c(0, 1))+
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "none"
  )

gcomb <- ggarrange(g1, g4, g3,  g2, ncol=2)

tikz(file = "figure_stoch_bifur.tex", width = 6, height = 4, standAlone = T)
plot(gcomb)
dev.off()
tools::texi2dvi('figure_stoch_bifur.tex', pdf = T, clean = T)
