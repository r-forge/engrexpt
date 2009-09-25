###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: choose42
###################################################
choose(4, 2)


###################################################
### chunk number 3: probylt2
###################################################
sum(dbinom(0:1, 50, 0.03))


###################################################
### chunk number 4: binomplotdisply eval=FALSE
###################################################
## xyplot(dbinom(0:50, size = 50, prob = 0.5) ~ 0:50, type = "h")


###################################################
### chunk number 5: binomplot
###################################################
show(xyplot(dbinom(0:50, size = 50, prob = 0.5) ~ 0:50, type = "h", xlab = NULL))


###################################################
### chunk number 6: binomplot2
###################################################
show(xyplot(dbinom(0:50, 50, 0.07) + dbinom(0:50, 50, 0.93) ~ 0:50, type = "h", outer = TRUE, xlab = NULL, ylab = NULL, layout = c(1,2)))


