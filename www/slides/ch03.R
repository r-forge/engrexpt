###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: dhazefm1
###################################################
fm1 <- aov(dhaze ~ treatment, dhaze)
summary(fm1)
model.tables(fm1, type = "means")
str(resid(fm1))


###################################################
### chunk number 3: fm2
###################################################
fm2 <- aov(lw ~ comp2 + comp1, lw)


###################################################
### chunk number 4: fm2summary
###################################################
summary(fm2)


###################################################
### chunk number 5: fm2table
###################################################
model.tables(fm2)
str(resid(fm2))


###################################################
### chunk number 6: ovendot
###################################################
show(dotplot(type ~ moisture|reorder(brand,moisture), oven,
             type = c("p","a"), pch=21,
             xlab = "Percentage of moisture remaining in silica after 15 min. drying",
             auto.key = list(columns = 3, lines = TRUE),
             jitter.y = TRUE, strip=FALSE, strip.left=TRUE,
             layout = c(1,3)))


###################################################
### chunk number 7: fm3
###################################################
summary(fm3 <- aov(moisture ~ type * brand, oven))
str(fitted(fm3))


###################################################
### chunk number 8: ex331
###################################################
ex331 <- data.frame(x = c(2,9,3,5,1), y = c(1,17,3,9,0))


###################################################
### chunk number 9: ex331plot
###################################################
show(xyplot(y ~ x, ex331, type = c("g","p","r"), aspect = 'xy'))


###################################################
### chunk number 10: fm4
###################################################
summary(fm4 <- lm(y ~ x, ex331))


###################################################
### chunk number 11: ex333
###################################################
ex333 <-
    data.frame(x = c(9,2,5,8,4,1,3,7,6),
               y = c(253.5,7.1,74.1,165.2,32.,1.9,18.1,136.1,100))
print(xyplot(y ~ x, ex333, type = c("g","p")),
      split = c(1,1,2,1), more = TRUE)
print(xyplot(y ~ x, ex333,
             type = c("g","p","r"), scales = list(log = 2)),
      split = c(2,1,2,1))


###################################################
### chunk number 12: fm5
###################################################
summary(fm5 <- lm(log(y) ~ log(x), ex333))


###################################################
### chunk number 13: ex336
###################################################
ex336 <- data.frame(x = c(18,18,20,20,22,22,24,24,26,26),
           y = c(4.0,4.2,5.6,6.1,6.5,6.8,5.4,5.6,3.3,3.6))


###################################################
### chunk number 14: ex336fig
###################################################
show(xyplot(y ~ x, ex336, type = c("g","p"), aspect = 0.8))


###################################################
### chunk number 15: fm6
###################################################
summary(fm6 <- lm(y ~ x + I(x^2), ex336))


###################################################
### chunk number 16: yield
###################################################
str(yield)


###################################################
### chunk number 17: fm7
###################################################
summary(fm7 <- lm(yield ~ temp + pH, yield))


###################################################
### chunk number 18: fm8
###################################################
fm8 <- lm(time ~ temp, timetemp, subset = type == "Repaired")


###################################################
### chunk number 19: fm8rp1
###################################################
plot(fm8, which = 1)


###################################################
### chunk number 20: fm8resf eval=FALSE
###################################################
## xyplot(resid(fm8) ~ fitted(fm8), type = c("g","p","smooth"))


###################################################
### chunk number 21: fm8resfshow
###################################################
show(xyplot(resid(fm8) ~ fitted(fm8),
            type = c("g","p","smooth"), aspect = 0.8))


###################################################
### chunk number 22: fig310 eval=FALSE
###################################################
## xyplot(resid(fm8) ~ temp, timetemp, subset = type == "Repaired")


###################################################
### chunk number 23: fig310fshow
###################################################
show(xyplot(resid(fm8) ~ temp, timetemp,
            subset = type == "Repaired",
            type = c("g","p"), aspect = 0.8,
            xlab = "Temperature", ylab = "Residuals"))


###################################################
### chunk number 24: shaker
###################################################
shaker <-
    data.frame(leftover = c(6.3,6.1,5.8,5.9,5.6,5.3,6.1,5.8,5.5),
               flowrate = rep(c(85,90,95), 3),
               vacuum = rep(c(20,22,24), each = 3))


###################################################
### chunk number 25: shakerdataplots
###################################################
print(xyplot(leftover ~ vacuum, shaker,
             type = c("g","b"), groups = flowrate,
             auto.key = list(columns = 3, lines = TRUE)),
      split = c(1,1,2,1), more = TRUE)
print(xyplot(leftover ~ flowrate, shaker,
             type = c("g","b"), groups = vacuum,
             auto.key = list(columns = 3, lines = TRUE)),
      split = c(2,1,2,1))


###################################################
### chunk number 26: fm9 eval=FALSE
###################################################
## summary(fm9 <- lm(leftover ~ flowrate + vacuum + I(vacuum^2),
##                   shaker))


###################################################
### chunk number 27: fm9cap
###################################################
foo <- capture.output(summary(fm9 <- lm(leftover ~ flowrate + vacuum + I(vacuum^2), shaker)))


###################################################
### chunk number 28: fm9out
###################################################
cat(paste(foo[-(1:10)], collapse = "\n"), "\n")


###################################################
### chunk number 29: corr
###################################################
with(subset(timetemp, type == "Repaired"), cor(time, temp))


