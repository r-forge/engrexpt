###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: adhesion2
###################################################
show(dotplot(cat ~ adhesion, adhesion2, groups = pH,
             type = c("p","a"), pch = 21, jitter.y = TRUE,
             ylab = "Catalyst",
             auto.key = list(space = "right", title = "pH",
                             lines = TRUE, points = FALSE)))


###################################################
### chunk number 3: adhesion2
###################################################
summary(fm1 <- aov(adhesion ~ cat * factor(pH), adhesion2))


###################################################
### chunk number 4: fm2
###################################################
summary(fm2 <- aov(adhesion ~ cat + factor(pH), adhesion2))


###################################################
### chunk number 5: fm2HSD
###################################################
TukeyHSD(fm2, which = "factor(pH)")


###################################################
### chunk number 6: adhesion2plt2
###################################################
show(xyplot(adhesion ~ pH, adhesion2, groups = cat, type = c("g","p","a"),
            aspect = 1.1, auto.key = list(space = "right", lines = TRUE)))


###################################################
### chunk number 7: fm3 eval=FALSE
###################################################
## summary(fm3 <- lm(adhesion ~ pH * cat, adhesion2))


###################################################
### chunk number 8: fm3show
###################################################
cat(paste(capture.output(summary(fm3 <- lm(adhesion ~ pH * cat, adhesion2)))[-(1:8)], collapse = "\n"))


###################################################
### chunk number 9: fm3anova
###################################################
anova(fm3)


###################################################
### chunk number 10: fm4 eval=FALSE
###################################################
## summary(fm4 <- lm(adhesion ~ pH + cat, adhesion2))


###################################################
### chunk number 11: fm4show
###################################################
cat(paste(capture.output(summary(fm4 <- lm(adhesion ~ pH + cat, adhesion2)))[-(1:8)], collapse = "\n"))


###################################################
### chunk number 12: fm5
###################################################
ad2 <- within(adhesion2, {opH <- ordered(pH); ocat <- ordered(cat)})
anova(fm5 <- lm(adhesion ~ ocat * opH, ad2))


###################################################
### chunk number 13: fm6 eval=FALSE
###################################################
## summary(fm6 <- lm(adhesion ~ ocat + opH, ad2))


###################################################
### chunk number 14: fm6show
###################################################
cat(paste(capture.output(
summary(fm6 <- lm(adhesion ~ ocat + opH, ad2))
)[-(1:8)], collapse = "\n"))


###################################################
### chunk number 15: computerplt
###################################################
show(dotplot(type ~ time, computer, groups = brand,
             pch = 21, jitter.y = TRUE, type = c("p","a"),
             xlab = "Repair time (minutes)",
             auto.key = list(space = "right", lines = TRUE, title = "Brand")
             ))


###################################################
### chunk number 16: fm7
###################################################
## compare to anova table on p. 316
summary(fm7 <- aov(time ~ brand * type, computer)) 


###################################################
### chunk number 17: fm89
###################################################
## remove insignificant interaction term
summary(fm8 <- aov(time ~ brand + type, computer))
## remove insignificant brand term
summary(fm9 <- aov(time ~ type, computer))


###################################################
### chunk number 18: 
###################################################
op <- options(width = 80)


###################################################
### chunk number 19: fm9HSD
###################################################
TukeyHSD(fm9)


###################################################
### chunk number 20: 
###################################################
options(op)


