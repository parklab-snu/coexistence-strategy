library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family="Times"))
library(tikzDevice)
source("../R/calculate_nf_stoch.R")

amat_pd <- matrix(c(0.4, -0.4, 1, 0), 2, 2, byrow=TRUE)
amat_hg <- matrix(c(1, 0.4, 0.4, 0), 2, 2, byrow=TRUE)
amat_co <- matrix(c(1, -0.4, 0.4, 0), 2, 2, byrow=TRUE)
amat_sd <- matrix(c(0.4, 0.4, 1, 0), 2, 2, byrow=TRUE)

kvec <- seq(0, 4, length.out=21)

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

xlim <- c(-3, 1)
ylim <- c(-2, 2)

coex_region <- data.frame(
  nd = seq(xlim[1], xlim[2], length.out = 500)
) %>%
  mutate(
    ymin = pmax(log(1 - nd), ylim[1]),
    ymax = pmin(-log(1 - nd), ylim[2])
  )

g1 <- ggplot(nf_pd_k) +
  geom_ribbon(data = coex_region, aes(x = nd, ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = nd, y = log(1 - nd)), lty=2) +
  geom_line(data = coex_region, aes(x = nd, y = -log(1 - nd)), lty=2) +
  geom_hline(yintercept=0, lty=3) +
  geom_vline(xintercept=0, lty=3) +
  geom_path(aes(nd, log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_pd_k, k==0), aes(nd, log(fd)), size=4) + 
  geom_text(data=filter(nf_pd_k, k==0), aes(nd, log(fd), label="PD"), vjust=-2, family="Times") + 
  geom_path(data=nf_hg_k, aes(nd, log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_hg_k, k==0), aes(nd, log(fd)), size=4) + 
  geom_text(data=filter(nf_hg_k, k==0), aes(nd, log(fd), label="HG"), vjust=2, family="Times") + 
  geom_path(data=nf_co_k, aes(nd, log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_co_k, k==0), aes(nd, log(fd)), size=4) + 
  geom_text(data=filter(nf_co_k, k==0), aes(nd, log(fd), label="SH"), vjust=-2, family="Times") + 
  geom_path(data=nf_sd_k, aes(nd, log(fd)), 
            lwd=1, arrow = arrow(length=unit(0.1, "inches"))) +
  geom_point(data=filter(nf_sd_k, k==0), aes(nd, log(fd)), size=4) + 
  geom_text(data=filter(nf_sd_k, k==0), aes(nd, log(fd), label="HD"), vjust=2, family="Times") + 
  scale_x_continuous("Niche difference, $1-\\rho$",
                     expand=c(0, 0)) +
  scale_y_continuous("Fitness difference, $\\log(\\kappa_D/\\kappa_C)$",
                     expand=c(0, 0)) +
  coord_cartesian(ylim=c(ylim[1], ylim[2]), xlim=c(-1, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "top"
  )

tikz(file = "figure_stoch.tex", width = 4, height = 4, standAlone = T)
plot(g1)
dev.off()
tools::texi2dvi('figure_stoch.tex', pdf = T, clean = T)
