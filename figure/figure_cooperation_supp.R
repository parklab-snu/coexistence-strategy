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
