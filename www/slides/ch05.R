###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: umeansim
###################################################
mns5 <- replicate(50000, mean(runif(5, min = -1, max = 1)))


###################################################
### chunk number 3: umeanshistshow eval=FALSE
###################################################
## histogram(~mns5,breaks = seq(-1, 1, len = 40))


###################################################
### chunk number 4: umeanshist
###################################################
print(histogram(~mns5,xlab=NULL,breaks = seq(-1, 1, len = 40)))


###################################################
### chunk number 5: moremeans
###################################################
mns1 <- runif(50000, -1, 1)
mns10 <- replicate(50000, mean(runif(10, -1, 1)))
mns20 <- replicate(50000, mean(runif(20, -1, 1)))
sapply(list(mns1, mns5, mns10, mns20), mean)
sapply(list(mns1, mns5, mns10, mns20), var)


###################################################
### chunk number 6: mnshist
###################################################
print(histogram(~ mns1 + mns5 + mns10 + mns20, outer = TRUE,
                breaks = seq(-1,1,len = 40), layout = c(4,1),
                xlab = "Means of samples of size n from U(-1,1)"))


###################################################
### chunk number 7: qqmathrunif
###################################################
print(qqmath(~ mns1 + mns5 + mns10 + mns20, outer = TRUE,
             f.value = ppoints(200), layout = c(4,1), ylab = NULL,
             xlab = "Means of samples of size n from U(-1,1)"))



###################################################
### chunk number 8: qqmathoverlaid
###################################################
show(qqmath(~ mns1 + mns5 + mns10 + mns20, outer = FALSE, type = c("g","l"),
            ylab = NULL, aspect = 1, f.values = ppoints(200),
            xlab = "Standard normal quantiles",
            auto.key = list(columns = 4, lines = TRUE, points = FALSE)))


###################################################
### chunk number 9: emns
###################################################
emns01 <- replicate(50000, mean(rexp(1, rate = 1/7)))
emns05 <- replicate(50000, mean(rexp(5, rate = 1/7)))
emns15 <- replicate(50000, mean(rexp(15, rate = 1/7)))
emns50 <- replicate(50000, mean(rexp(50, rate = 1/7)))


###################################################
### chunk number 10: eqqmathoverlaid
###################################################
show(qqmath(~ emns01 + emns05 + emns15 + emns50,
            outer = TRUE, type = c("g","l"),
            ylab = NULL, aspect = 1, f.values = ppoints(200),
            xlab = "Standard normal quantiles",
            scales = list(y = list(relation = "free"))))


###################################################
### chunk number 11: 
###################################################
8 * qnorm(0.025)^2


###################################################
### chunk number 12: qnorm025
###################################################
qnorm(0.025, low = FALSE)


###################################################
### chunk number 13: tdensity
###################################################
xv <- seq(-4.5, 4.5, 0.01)
print(xyplot(dnorm(xv)+dt(xv,25)+dt(xv,10)+dt(xv,5) ~ xv,
             scales = list(x = list(axs = "i")),
             ylab = NULL, xlab = NULL, type = c("g", "l"),
             auto.key = list(text = expression(Z, T[25], T[10], T[5]),
             points = FALSE, lines = TRUE, columns = 4)))


###################################################
### chunk number 14: opdig
###################################################
op <- options(digits=5)


###################################################
### chunk number 15: times
###################################################
charge <- c(5.11,2.1,4.27,5.04,4.47,3.73,5.96,6.21)
c(summary(charge), sd = sd(charge))
t.test(charge)


###################################################
### chunk number 16: chargeplot
###################################################
print(densityplot(charge,xlab = "Discharge times"),
      pos = c(0,0,0.65,1), more = TRUE)
print(qqmath(charge,type=c("g","p"), aspect=1, ylab = NULL,
             xlab = NULL), pos = c(0.65,0,1,1))


###################################################
### chunk number 17: thickplot
###################################################
print(qqmath(~ thickness, ccthickn, aspect = 1,
             xlab = "Standard normal quantiles"),
      pos = c(0,0,0.40,1), more = TRUE)
print(densityplot(~thickness, ccthickn),
      pos = c(0.40,0,1,1))


###################################################
### chunk number 18: assaydat
###################################################
with(ccthickn, c(summary(thickness), sd = sd(thickness)))
with(ccthickn, t.test(thickness, mu = 65, conf = 0.9))


###################################################
### chunk number 19: unopt
###################################################
options(op)


###################################################
### chunk number 20: sampsz
###################################################
ceiling((qnorm(0.025)*0.4/0.2)^2)


###################################################
### chunk number 21: sampreal
###################################################
ceiling(uniroot(function(x) x-(qt(.025,x-1)*0.4/0.2)^2,
                c(2,100))$root)


