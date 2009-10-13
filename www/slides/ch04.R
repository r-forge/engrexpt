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


###################################################
### chunk number 7: poisplot
###################################################
show(xyplot(dpois(0:15,lambda=3) ~ 0:15, type = "h", xlab = NULL))


###################################################
### chunk number 8: poisapprox
###################################################
c(binom = sum(dbinom(0:1, 50, 0.03)), Poisson = sum(dpois(0:1, 1.5)))
all.equal(sum(dbinom(0:1, 50, 0.03)), sum(dpois(0:1, 1.5)))


###################################################
### chunk number 9: geomplot
###################################################
show(xyplot(dgeom(0:200, prob = 0.03) ~ 0:200, type = "h", xlab = NULL))


###################################################
### chunk number 10: geomprob
###################################################
sum(dgeom(0:2, prob = 0.03))
pgeom(2, prob = 0.03)


###################################################
### chunk number 11: set.seed
###################################################
set.seed(1234321)


###################################################
### chunk number 12: samplegeom
###################################################
mean(gsamp <- rgeom(1000000, prob = 0.03)) # should be near 32.3
var(gsamp)  # should be near 0.97/0.03^2 = 1077.778


###################################################
### chunk number 13: unifcdfplot
###################################################
eps <- 100*.Machine$double.eps
y <- c(12.5,13-eps, 13.0,15.0,15 + eps, 15.5)
print(xyplot(dunif(y, min = 13, max = 15) + punif(y, min = 13, max = 15) ~ y,
             outer = TRUE, type = c("g","l"), xlab = "y", ylab = NULL, layout = c(1,2),
             scales = list(y = list(relation = "free"), x = list(axs = "i"))),
      pos = c(0,0,0.5,1), more = TRUE)
y <- c(-0.1,-eps, 0,1,1+eps, 1.1)
print(xyplot(dunif(y, min = 0, max = 1) + punif(y, min = 0, max = 1) ~ y,
             outer = TRUE, type = c("g","l"), xlab = "y", ylab = NULL, layout = c(1,2),
             scales = list(y = list(relation = "free"), x = list(axs = "i"))),
      pos = c(0.5,0,1,1))


###################################################
### chunk number 14: pexp
###################################################
pexp(3.5, rate = 1/7)


###################################################
### chunk number 15: exp6
###################################################
1 - pexp(6, rate = 1/7)
pexp(6, 1/7, lower = FALSE)


###################################################
### chunk number 16: exp3.5
###################################################
pexp(9, 1/7) - pexp(3.5, 1/7)
diff(pexp(c(3.5, 9), 1/7))


###################################################
### chunk number 17: stdnormdensshow eval=FALSE
###################################################
## xyplot(dnorm(z) ~ z, type = c("g","l"))


###################################################
### chunk number 18: stdnormdens
###################################################
z <- seq(-3.65, 3.65, 0.01)
print(xyplot(dnorm(z) ~ z, type = c("g","l"), scales = list(x = list(axs = 'i'))))


###################################################
### chunk number 19: IQnormdens
###################################################
x <- seq(47, 153, 0.5)
print(xyplot(dnorm(x, mean = 100, sd = 15) ~ x, type = c("g","l"), scales = list(x = list(axs = 'i'))))


###################################################
### chunk number 20: IQnormcdf
###################################################
print(xyplot(pnorm(x, mean = 100, sd = 15) ~ x, type = c("g","l"), scales = list(x = list(axs = 'i'))))


###################################################
### chunk number 21: pnorm
###################################################
pnorm(500, 450, 30)
1 - pnorm(550, 450, 30)
diff(pnorm(c(400,500), 450, 30))


###################################################
### chunk number 22: qnorm
###################################################
qnorm(0.98, 100, 15)


###################################################
### chunk number 23: alumdens
###################################################
print(densityplot(~ ppm, alum, xlab = "Parts per million"),
      split = c(1,1,1,2), more = TRUE)
print(densityplot(~ ppm, alum, scales = list(x = list(log = 2)),
                  xlab = "Parts per million (logarithmic scale)"),
      split = c(1,2,1,2))


###################################################
### chunk number 24: pars
###################################################
with(alum, c(muhat = mean(log(ppm)), sigmahat = sd(log(ppm))))


###################################################
### chunk number 25: ex4.3.20
###################################################
plnorm(100, meanlog = 5.0999591, sdlog = 0.6958257)
pnorm(log(100), mean = 5.0999591, sd = 0.6958257)


###################################################
### chunk number 26: weibullplot
###################################################
w <- seq(0, 5, 0.02)
show(xyplot(dweibull(w, 0.5, 1) + dweibull(w, 1, 1) + dweibull(w, 1.5, 1) + dweibull(w, 3, 1) ~ w,
            type = c("g","l"),
            outer = FALSE, ylab = NULL, xlab = NULL,
            auto.key = list(columns = 4, lines = TRUE, points = FALSE)))


