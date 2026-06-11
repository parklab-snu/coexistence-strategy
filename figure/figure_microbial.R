library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(tikzDevice)
library(egg)

alpha <- 0.15

## panel A

fvec <- seq(0, 1, length.out=201)
epsilon_A <- 0.4
c_A <- 0.3

pdiff_all <- bind_rows(
  data.frame(
    f=fvec,
    pdiff=epsilon_A-c_A,
    group="Linear"
  ),
  data.frame(
    f=fvec,
    pdiff=(epsilon_A+fvec*(1-epsilon_A))^alpha-c_A-(fvec*(1-epsilon_A))^alpha,
    group="Nonlinear"
  )
)

g1 <- ggplot(pdiff_all) +
  geom_hline(yintercept=0, lty=3) +
  geom_line(aes(f, pdiff, lty=group), col="#00BBFF", lwd=1.5) +
  scale_x_continuous("Fraction of cooperations, $f$",
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels=c("0", 0.2, 0.4, 0.6, 0.8, "1"),
                     expand=c(0,0)) +
  scale_y_continuous("Payoff difference, $\\pi_C(f)-\\pi_D(f)$",
                     breaks=c(0, 0.2, 0.4, 0.6),
                     labels=c("0", 0.2, 0.4, 0.6)) +
  scale_linetype_manual(values=c(1, 2)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.7, 0.8),
    legend.title = element_blank(),
    panel.border = element_rect(linewidth=2),
    legend.background = element_rect(fill=NA)
  )

alphavec_B <- seq(0, 1, length.out=201)
epsilonvec_B <- c(0.2, 0.4, 0.6)

nd_data_B <- lapply(epsilonvec_B, function(x) {
  
  data.frame(
    alpha=alphavec_B,
    nd=1-exp((1-x^alphavec_B-(1-x)^alphavec_B)/2),
    fd=exp(c_A + ((1-x)^alphavec_B-1-x^alphavec_B)/2),
    epsilon=x
  )
  
}) %>%
  bind_rows %>%
  mutate(
    epsilon=factor(epsilon,
                   levels=c(0.6, 0.4, 0.2))
  )

g2 <- ggplot(nd_data_B) +
  geom_line(aes(alpha, nd, lty=epsilon), col="#EF6351", lwd=1.5) +
  scale_x_continuous("Nonlinearity, $\\alpha$",
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     labels=c("0", 0.2, 0.4, 0.6, 0.8, "1"),
                     expand=c(0,0)) +
  scale_y_continuous("Niche difference, $1-\\rho$",
                     limits=c(0, 0.5),
                     breaks=c(0, 0.2, 0.4, 0.6),
                     labels=c("0", 0.2, 0.4, 0.6),
                     expand=c(0, 0)) +
  scale_linetype_discrete("Efficiency, $\\epsilon$") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.9, 0.7),
    panel.border = element_rect(linewidth=2),
    legend.key.width = unit(2, "line"),
    legend.background = element_rect(fill=NA)
  )

xlim <- c(0, 0.5)
ylim <- c(-0.4, 0.4)

coex_region <- data.frame(
  nd = seq(xlim[1], xlim[2], length.out = 500)
) %>%
  mutate(
    ymin = pmax(log(1 - nd), ylim[1]),
    ymax = pmin(-log(1 - nd), ylim[2])
  )

g3 <- ggplot(nd_data_B) +
  geom_ribbon(data = coex_region, aes(x = nd, ymin = ymin, ymax = ymax),
              fill = "gray90") +
  geom_line(data = coex_region, aes(x = nd, y = log(1 - nd)), lty=2) +
  geom_line(data = coex_region, aes(x = nd, y = -log(1 - nd)), lty=2) +
  geom_hline(yintercept=0, lty=3) +
  geom_path(aes(nd, log(fd), lty=epsilon), col="#EF6351", lwd=1.5) +
  annotate("point", x=1-exp(-1/2), y=c_A-1/2, size=1, 
           shape=22,
           fill="gray90", stroke=2, col="#EF6351") +
  annotate("text", x=1-exp(-1/2), y=c_A-1/2, label="$\\alpha=0$",
           hjust=-0.25, vjust=0.4,
           col="#EF6351",
           size=3) +
  geom_path(data=nd_data_B %>% filter(epsilon==0.6, nd > 0.1, nd < 0.2),
            aes(nd, log(fd)-0.03),
            col="#EF6351",
            arrow = arrow(type = "closed", length = unit(0.06, "inches"))
            ) +
  annotate("text", x=0.15, y=-0.35, label="Increase in $\\alpha$",
           hjust=0.6, vjust=0,
           col="#EF6351",
           size=3) +
  geom_point(data=nd_data_B %>% filter(alpha==0.15), aes(nd, log(fd)),
             size=1, col="#EF6351",
             shape=21,
             fill="gray90", stroke=2) +
  annotate("text", x=0.3, y=-0.1, label="$\\alpha=0.15$",
           hjust=-0.1, vjust=-1.1,
           col="#EF6351",
           size=3) +
  scale_x_continuous("Niche difference, $1-\\rho$",
                     limits=c(0, 0.5),
                     breaks=c(0, 0.2, 0.4),
                     labels=c("0", 0.2, 0.4),
                     expand=c(0, 0)) +
  scale_y_continuous("Fitness difference, $\\log(\\kappa_D/\\kappa_C)$",
                     breaks=c(-0.4, -0.2, 0, 0.2, 0.4),
                     labels=c(-0.4, -0.4, "0", 0.2, 0.4),
                     limits=c(-0.4, 0.4),
                     expand=c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    panel.border = element_rect(linewidth=2)
  )

gcomb <- ggarrange(g1, g2, g3, nrow=1,
                   labels=c("A", "B", "C"))

tikz(file = "figure_microbial.tex", width = 9/1.2, height = 3, standAlone = T)
plot(gcomb)
dev.off()
tools::texi2dvi('figure_microbial.tex', pdf = T, clean = T)
