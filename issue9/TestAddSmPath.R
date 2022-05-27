remove(list=ls())
source("AddSmPath.R")

dhat = c(-.5,-.25,-.24,-.3,-.05,0,1.25)
Vhat = diag(c(.125,.125,.125,.125,.125,0,.125)^2)
p    = length(dhat)

invVhat = pinv(Vhat)

# qchisq(0.95, p)
# PolyWaldMin(dhat, invVhat, 6, 0)$W
# PolyWaldMin(dhat, invVhat, 6, 1)$W
# PolyWaldMin(dhat, invVhat, 6, 2)$W
# PolyWaldMin(dhat, invVhat, 6, 3)$W
#
# FindOrder(dhat, invVhat, qchisq(0.95, p), 10)


sm_path <- AddSmPath(dhat, Vhat)

df <- data.frame(k    = seq(1, length(dhat)),
                 d    = dhat,
                 se   = diag(Vhat)^.5,
                 path = sm_path)

df$d_lb = df$d - 1.96*df$se
df$d_ub = df$d + 1.96*df$se

ggplot2::ggplot(df) +
  geom_point(aes(y = d, x = k),
             color = 'red') +
    geom_point(aes(y = d_lb, x = k),
               color = 'red', shape = 3) +
    geom_point(aes(y = d_ub, x = k),
               color = 'red', shape = 3) +
  geom_point(aes(y = path, x = k)) +
  theme_bw()

ggsave("test.png", dpi = 250, width = 7, height = 5)

