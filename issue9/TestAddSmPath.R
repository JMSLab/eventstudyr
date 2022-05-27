library(pracma) # install.packages("pracma")

dhat = c(-.5,-.25,-.24,-.3,-.05,0,1.25)
Vhat = diag(c(.125,.125,.125,.125,.125,0,.125)^2)
p    = length(dhat)

invVhat = pinv(Vhat)

qchisq(0.95, p)
PolyWaldMin(dhat, invVhat, 6, 0)$W
PolyWaldMin(dhat, invVhat, 6, 1)$W
PolyWaldMin(dhat, invVhat, 6, 2)$W
PolyWaldMin(dhat, invVhat, 6, 3)$W

FindOrder(dhat, invVhat, qchisq(0.95, p), 10)
