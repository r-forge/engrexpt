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
particle <-
    data.frame(amt = c(6.3,6.1,5.8,5.9,5.6,5.3,6.1,5.8,5.5),
               rate = rep(c(85,90,95),3),
               vacuum = rep(c(20,22,24), each = 3))
               
               


###################################################
### chunk number 21: strparticle
###################################################
str(particle)


###################################################
### chunk number 22: particleplot
###################################################
print(xyplot(amt ~ rate, particle, groups = vacuum,
             type = c("g","b"), xlab = "Flow Rate",
             ylab = "Amount of material",
             auto.key = list(space = "right", title = "Vacuum",
             lines = TRUE, points = FALSE)),
      pos = c(0,0,0.5,1), more = TRUE)
print(xyplot(amt ~ vacuum, particle, groups = rate,
             type = c("g","b"), xlab = "Vacuum",
             ylab = "Amount of material",
             auto.key = list(space = "right", title = "Flow rate",
             lines = TRUE, points = FALSE)),
      pos = c(0.5,0,1,1))


###################################################
### chunk number 23: fm10
###################################################
summary(fm10 <- aov(amt ~ factor(rate) + factor(vacuum), particle))


###################################################
### chunk number 24: fm11
###################################################
part2 <- within(particle, {ora <- ordered(rate); ovac <- ordered(vacuum)})
anova(fm11 <- lm(amt ~ ora + ovac, part2))


###################################################
### chunk number 25: fm11sumshow eval=FALSE
###################################################
## summary(fm11)


###################################################
### chunk number 26: fm11sum
###################################################
cat(paste(capture.output(summary(fm11))[-(1:9)], collapse = "\n"))


###################################################
### chunk number 27: fm12
###################################################
summary(fm12 <- aov(amt ~ rate + ovac, part2))
TukeyHSD(fm12, which = "ovac")


###################################################
### chunk number 28: uvstk
###################################################
str(uvstk <- within(stack(uvcoatin[,1:2]),
                    subj <- gl(10,1,20,labels=LETTERS[1:10])))


###################################################
### chunk number 29: t.test
###################################################
with(uvcoatin, t.test(a, b, paired = TRUE))
summary(fm13 <- aov(values ~ subj + ind, uvstk))


###################################################
### chunk number 30: strdeink2
###################################################
str(deink2)
xtabs(bright ~ formula + newspaper, deink2)


###################################################
### chunk number 31: deinkplt
###################################################
print(dotplot(reorder(formula, bright) ~ bright, deink2,
              groups = newspaper, type = c("p","a")))


###################################################
### chunk number 32: fm14
###################################################
summary(fm14 <- aov(bright ~ newspaper + formula, deink2))


###################################################
### chunk number 33: fm15
###################################################
summary(fm15 <- aov(bright ~ formula, deink2))


###################################################
### chunk number 34: deink1fplt
###################################################
print(dotplot(reorder(formula,bright) ~ bright, deink2, type = c("p","a"),
              pch = 21, jitter.y = TRUE))


###################################################
### chunk number 35: fm15Tukey
###################################################
TukeyHSD(fm15)


