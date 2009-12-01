###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: ex10.1.1show eval=FALSE
###################################################
## summary(fm1 <- lm(time ~ temp, timetemp,
##                   subset = type == "Repaired"))


###################################################
### chunk number 3: ex10.1.1
###################################################
cat(paste(capture.output(summary(fm1 <- lm(time ~ temp,
                                           timetemp,
                                           subset = type == "Repaired")))[-(1:8)],
          collapse = "\n"), "\n")


###################################################
### chunk number 4: ex10.1.2
###################################################
confint(fm1)


###################################################
### chunk number 5: fbuildplt
###################################################
fm2 <- lm(gloss ~ build, fbuild)
print(xyplot(gloss ~ build, fbuild, type = c("g","p","r"),
             xlab = "Film build", ylab = "gloss", aspect = 1),
      split = c(1,1,3,1), more = TRUE)
print(xyplot(resid(fm2) ~ fitted(fm2),
             type = c("g", "p", "smooth")),
      split = c(2,1,3,1), more = TRUE)
print(qqmath(~resid(fm2), aspect = 1, ylab = "Residuals",
             type = c("g", "p"), xlab = "Standard normal quantiles"),
      split = c(3,1,3,1))


###################################################
### chunk number 6: fm2prt
###################################################
cat(paste(capture.output(summary(fm2))[-(1:9)],
          collapse = "\n"), "\n")



###################################################
### chunk number 7: fm2confint
###################################################
confint(fm2)


###################################################
### chunk number 8: anovafm1
###################################################
anova(fm1)


###################################################
### chunk number 9: explicitanova
###################################################
anova(update(fm1, . ~ . - temp), fm1)


###################################################
### chunk number 10: coeftab
###################################################
coef(summary(fm1))


###################################################
### chunk number 11: fm2pred
###################################################
str(predict(fm2, list(build = 2.6), se.fit = TRUE)) # Ex 10.1.7


###################################################
### chunk number 12: predfm2
###################################################
predict(fm2, list(build = 2.6), int = "conf", level = 0.90)


###################################################
### chunk number 13: predfm2
###################################################
predict(fm2, list(build = 3:4), int = "pred", level = 0.90)


###################################################
### chunk number 14: phplt
###################################################
fm4 <- lm(phnew ~ phold, phmeas)
print(xyplot(phnew ~ phold, phmeas, type = c("g","p","r"),
             xlab = "pH by old method",
             ylab = "pH by new method", aspect = 1),
      split = c(1,1,3,1), more = TRUE)
print(xyplot(resid(fm4) ~ fitted(fm4),
             type = c("g", "p", "smooth")),
      split = c(2,1,3,1), more = TRUE)
print(qqmath(~resid(fm4), aspect = 1, ylab = "Residuals",
             type = c("g", "p"), xlab = "Standard normal quantiles"),
      split = c(3,1,3,1))


###################################################
### chunk number 15: fm2prt
###################################################
cat(paste(capture.output(summary(fm4))[-(1:9)],
          collapse = "\n"), "\n")


###################################################
### chunk number 16: lackoffit
###################################################
anova(fm4, lm(phnew ~ factor(phold), phmeas))


###################################################
### chunk number 17: ex336
###################################################
ex336 <- data.frame(x = c(18,18,20,20,22,22,24,24,26,26),
           y = c(4.0,4.2,5.6,6.1,6.5,6.8,5.4,5.6,3.3,3.6))


###################################################
### chunk number 18: partszplt
###################################################
fm5 <- lm(y ~ x + I(x^2), ex336)
print(xyplot(y ~ x, ex336, type = c("g","p"),
             xlab = "Vacuum setting",
             ylab = "Particle size", aspect = 1),
      split = c(1,1,3,1), more = TRUE)
print(xyplot(resid(fm5) ~ fitted(fm5),
             type = c("g", "p", "smooth")),
      split = c(2,1,3,1), more = TRUE)
print(qqmath(~resid(fm5), aspect = 1, ylab = "Residuals",
             type = c("g", "p"), xlab = "Standard normal quantiles"),
      split = c(3,1,3,1))


###################################################
### chunk number 19: fm5
###################################################
cat(paste(capture.output(summary(fm5))[-(1:9)],
          collapse = "\n"), "\n")


###################################################
### chunk number 20: confint
###################################################
confint(fm5)


###################################################
### chunk number 21: 
###################################################
predict(fm5, list(x = 21), int = "pred")
predict(fm5, list(x = 21), int = "conf")


###################################################
### chunk number 22: fm5lof
###################################################
anova(fm5, lm(y ~ factor(x), ex336))


