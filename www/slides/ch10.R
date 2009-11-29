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


