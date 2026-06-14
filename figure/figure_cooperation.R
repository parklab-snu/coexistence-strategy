library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tikzDevice)
library(egg)
source("../R/calculate_nf.R")

benefit <- 3
cost <- 1

Amat_basic <- matrix(
  c(benefit-cost, -cost,
    benefit, 0),
  2, 2, byrow=TRUE
)

nf_basic <- calculate_nf(Amat_basic)

rvec <- seq(0, 0.8, length.out=101)

nf_r <- lapply(rvec, function(r) {
  Amat_r <- matrix(
    c(
      (benefit-cost) * (1 + r), benefit * r - cost,
      benefit - r * cost, 0
    ),
    2, 2, byrow=TRUE
  )
  
  calculate_nf(Amat_r) %>%
    mutate(
      r=r
    )
}) %>%
  bind_rows %>%
  mutate(
    nd=0
  )

wvec <- seq(0, 0.5, length.out=101)

nf_w <- lapply(wvec, function(w) {
  Amat_w <- matrix(
    c(
      (benefit-cost)/(1 - w), - cost,
      benefit, 0
    ),
    2, 2, byrow=TRUE
  )
  
  calculate_nf(Amat_w) %>%
    mutate(
      w=w
    )
}) %>%
  bind_rows

qvec <- seq(0, 1, length.out=101)

nf_q <- lapply(qvec, function(q) {
  Amat_q <- matrix(
    c(
      (benefit-cost), - cost*(1-q),
      benefit * (1-q), 0
    ),
    2, 2, byrow=TRUE
  )
  
  calculate_nf(Amat_q) %>%
    mutate(
      q=q
    )
}) %>%
  bind_rows

kvec <- seq(2.5, 100, length.out=21)

nf_k <- lapply(kvec, function(k) {
  H <- ((benefit-cost) * k - 2 * cost)/((k+1)*(k-2))
  
  Amat_k <- matrix(
    c(
      (benefit-cost), H - cost,
      benefit - H, 0
    ),
    2, 2, byrow=TRUE
  )
  
  calculate_nf(Amat_k) %>%
    mutate(
      k=k
    )
}) %>%
  bind_rows

mvec <- c(2, 3, 4)
nvec <- seq(1, 5, length.out=21)

mndata <- expand.grid(mvec, nvec)

nf_mn <- lapply(1:nrow(mndata), function(x){ 
  pp <- mndata[x,]
  
  m <- pp[[1]]
  n <- pp[[2]]
  
  Amat_mn <- matrix(
    c(
      (benefit-cost) * (m+n), (benefit-cost) * m - cost * n,
      benefit * n, 0
    ),
    2, 2, byrow=TRUE
  )
  
  calculate_nf(Amat_mn) %>%
    mutate(
      m=m,
      n=n
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

g1 <- ggplot(nf_basic) +
  geom_ribbon(data = coex_region, aes(x = nd, ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = nd, y = log(1 - nd)), lty=2) +
  geom_line(data = coex_region, aes(x = nd, y = -log(1 - nd)), lty=2) +
  geom_hline(yintercept=0, lty=3) +
  geom_vline(xintercept=0, lty=3) +
  geom_point(data=nf_r %>% filter(r==0.8), aes(nd, log(fd)), size=4, col=4, fill=NA, stroke=1.5) +
  geom_point(data=nf_q %>% filter(q==1), aes(nd, log(fd)), size=4, col=3, fill=NA, stroke=1.5) +
  geom_point(data=nf_w %>% filter(w==0.5), aes(nd, log(fd)), size=4, col=2, fill=NA, stroke=1.5) +
  annotate("text", x=0.3, y=-1.4, label="$r=0.8$") +
  annotate("text", x=-1.6, y=-1.2, label="$q=1$") +
  annotate("text", x=-1.6, y=-0.25, label="$w=0.5$") +
  annotate("text", x=0, y=1.2, label="Prisoner's dilemma", hjust=1.1) +
  geom_path(data=nf_r, aes(nd, log(fd), col="Kin selection\n(genetic relatedness, $r$)"), 
            lwd=1.5, arrow = arrow(length=unit(0.2, "inches"))) +
  geom_path(data=nf_w %>% filter(nd > -2), aes(nd, log(fd), col="Direct reciprocity\n(probability of next round, $w$)"), 
            lwd=1.5, arrow = arrow(length=unit(0.2, "inches"))) +
  geom_path(data=nf_q, aes(nd, log(fd), col="Indirect reciprocity\n(social acquaintanceship, $q$)"), 
            lwd=1.5, arrow = arrow(length=unit(0.2, "inches"))) +
  geom_point(aes(nd, log(fd)), shape=21, size=4, fill="white", stroke=1.5) +
  scale_x_continuous("Niche difference, $1-\\rho$",
                     expand=c(0, 0)) +
  scale_y_continuous("Fitness difference, $\\log(\\kappa_D/\\kappa_C)$",
                     expand=c(0, 0)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  coord_cartesian(ylim=c(ylim[1], ylim[2]), xlim=c(-2, 1)) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.title = element_blank(),
    legend.position = "top"
  )

tikz(file = "figure_cooperation.tex", width = 4, height = 5, standAlone = T)
plot(g1)
dev.off()
tools::texi2dvi('figure_cooperation.tex', pdf = T, clean = T)

ggplot(nf_basic) +
  geom_ribbon(data = coex_region, aes(x = nd, ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = nd, y = log(1 - nd)), lty=2) +
  geom_line(data = coex_region, aes(x = nd, y = -log(1 - nd)), lty=2) +
  geom_point(aes(nd, log(fd))) +
  geom_path(data=nf_k, aes(nd, log(fd), col="Network reciprocity\n(number of neighbors, q)"))


ggplot(nf_basic) +
  geom_ribbon(data = coex_region, aes(x = nd, ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = nd, y = log(1 - nd)), lty=2) +
  geom_line(data = coex_region, aes(x = nd, y = -log(1 - nd)), lty=2) +
  geom_point(aes(nd, log(fd))) +
  geom_path(data=nf_mn, aes(nd, log(fd))) +
  facet_wrap(~m)
