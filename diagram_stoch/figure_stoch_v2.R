library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(tikzDevice)
source("../R/calculate_nf_stoch.R")

amat_pd <- matrix(c(1, -1, 8, 0), 2, 2, byrow=TRUE)
amat_hg <- matrix(c(8, 1, 1, 0), 2, 2, byrow=TRUE)
amat_co <- matrix(c(8, -1, 1, 0), 2, 2, byrow=TRUE)
amat_sd <- matrix(c(1, 1, 8, 0), 2, 2, byrow=TRUE)

kvec <- seq(0, 2, length.out=801)

nf_pd_k <- lapply(kvec, function(k) {
  calculate_nf_stoch(amat_pd, k) %>%
    mutate(
      k=k
    )
}) %>%
  bind_rows

nf_hg_k <- lapply(kvec, function(k) {
  calculate_nf_stoch(amat_hg, k) %>%
    mutate(
      k=k
    )
}) %>%
  bind_rows

nf_co_k <- lapply(kvec, function(k) {
  calculate_nf_stoch(amat_co, k) %>%
    mutate(
      k=k
    )
}) %>%
  bind_rows

nf_sd_k <- lapply(kvec, function(k) {
  calculate_nf_stoch(amat_sd, k) %>%
    mutate(
      k=k
    )
}) %>%
  bind_rows

xlim <- c(-200, 200)
ylim <- c(-60, 60)

coex_region <- data.frame(
  rho = exp(seq(xlim[1], xlim[2], length.out = 500))
) %>%
  mutate(
    ymin = pmax(log(rho), ylim[1]),
    ymax = pmin(-log(rho), ylim[2])
  )

g1 <- ggplot(nf_pd_k) +
  geom_ribbon(data = coex_region, aes(x = -log(rho), ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = -log(rho), y = log(rho)), lty=2) +
  geom_line(data = coex_region, aes(x = -log(rho), y = -log(rho)), lty=2) +
  geom_hline(yintercept=0, lty=3) +
  geom_vline(xintercept=0, lty=3) +
  geom_path(aes(-log(rho), log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_pd_k, k==0), aes(-log(rho), log(fd)), size=4) + 
  geom_text(data=filter(nf_pd_k, k==0), aes(-log(rho), log(fd), label="PD"), vjust=-2, family="Times") + 
  geom_path(data=nf_hg_k, aes(-log(rho), log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_hg_k, k==0), aes(-log(rho), log(fd)), size=4) + 
  geom_text(data=filter(nf_hg_k, k==0), aes(-log(rho), log(fd), label="HG"), vjust=2, family="Times") + 
  geom_path(data=nf_co_k, aes(-log(rho), log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_co_k, k==0), aes(-log(rho), log(fd)), size=4) + 
  geom_text(data=filter(nf_co_k, k==0), aes(-log(rho), log(fd), label="SH"), vjust=-1, family="Times") + 
  geom_path(data=nf_sd_k, aes(-log(rho), log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_sd_k, k==0), aes(-log(rho), log(fd)), size=4) + 
  geom_text(data=filter(nf_sd_k, k==0), aes(-log(rho), log(fd), label="HD"), vjust=2, hjust=1, family="Times") + 
  scale_x_continuous("Niche difference, $-\\log(\\rho)$",
                     expand=c(0, 0)) +
  scale_y_continuous("Fitness difference, $\\log(\\kappa_D/\\kappa_C)$",
                     expand=c(0, 0)) +
  coord_cartesian(ylim=c(ylim[1], ylim[2]), xlim=c(-10, 60)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "top"
  )

tikz(file = "figure_stoch_v2.tex", width = 3, height = 3, standAlone = T)
plot(g1)
dev.off()
tools::texi2dvi('figure_stoch_v2.tex', pdf = T, clean = T)
