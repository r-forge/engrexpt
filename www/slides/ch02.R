###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: railcarstr
###################################################
library(EngrExpt)
str(railcar)


###################################################
### chunk number 3: railcarhist
###################################################
show(histogram(~ days, railcar))


###################################################
### chunk number 4: railcarhistshow eval=FALSE
###################################################
## histogram(~ days, railcar)


###################################################
### chunk number 5: railcarhist2show eval=FALSE
###################################################
## histogram(~ days, railcar, breaks = seq(2, 44, 3), xlab = "Days")


###################################################
### chunk number 6: railcarhist2
###################################################
show(histogram(~ days, railcar, breaks = seq(2, 44, 3), xlab = "Days"))


###################################################
### chunk number 7: raildatadensityplot eval=FALSE
###################################################
## densityplot(~ days, railcar, xlab = "Days")


###################################################
### chunk number 8: raildensity
###################################################
show(densityplot(~ days, railcar, xlab = "Days"))


###################################################
### chunk number 9: raildensitylog
###################################################
show(densityplot(~ days, railcar, xlab = "Days",
                 scales = list(x = list(log = 2))))


###################################################
### chunk number 10: surfareastr
###################################################
str(surfarea)
summary(surfarea)
## alternative form for univariate data
with(surfarea, summary(area))


###################################################
### chunk number 11: surfareadensplot
###################################################
show(densityplot(~ area, surfarea,
                 xlab = "Surface area of samples of silica"))


###################################################
### chunk number 12: helptimetemp eval=FALSE
###################################################
## ?timetemp


###################################################
### chunk number 13: strtimetemp
###################################################
str(timetemp)


###################################################
### chunk number 14: OEMpanels
###################################################
OEM <- subset(timetemp, type == "OEM")


###################################################
### chunk number 15: xyplotOEM eval=FALSE
###################################################
## xyplot(time ~ temp, OEM)


###################################################
### chunk number 16: OEMplot
###################################################
show(xyplot(time ~ temp, OEM,
            aspect = 'xy',
            ylab = "Time to reach -10C (min.)",
            xlab = "Temperature in freezer (degrees C)"))


###################################################
### chunk number 17: xyplotOEM2 eval=FALSE
###################################################
## xyplot(time ~ temp, timetemp, subset = type == "OEM")


###################################################
### chunk number 18: xyplotbothshow eval=FALSE
###################################################
## xyplot(time ~ temp, timetemp, groups = type, type = c("g","p","r"))


###################################################
### chunk number 19: xyplotboth
###################################################
show(xyplot(time ~ temp, timetemp, groups = type,
            type = c("g","p","r"), aspect = 'xy',
            ylab = "Time to reach -10C (min.)",
            xlab = "Temperature in freezer (degrees C)",
            auto.key = list(columns = 2, lines = TRUE))
    )


###################################################
### chunk number 20: xyplotsepshow eval=FALSE
###################################################
## xyplot(time ~ temp | type, timetemp, type = c("g","p","r"))


###################################################
### chunk number 21: xyplotsep
###################################################
show(xyplot(time ~ temp | type, timetemp, type = c("g","p","r"), 
            ylab = "Time to reach -10C (min.)",
            xlab = "Temperature in freezer (degrees C)")
     )


###################################################
### chunk number 22: runsshow eval=FALSE
###################################################
## xyplot(absorb ~ seq_along(absorb), absorb, type = "b")


###################################################
### chunk number 23: runsshow
###################################################
show(xyplot(absorb ~ seq_along(absorb), absorb, type = "b",
            xlab = "Sample number within shift",
            ylab = "Oil absorption"))


###################################################
### chunk number 24: summaryout
###################################################
with(subset(timetemp, type == "OEM"), summary(time))
with(railcar, summary(days))
with(surfarea, summary(area))


###################################################
### chunk number 25: samplestats
###################################################
with(surfarea,
     c(IQR = IQR(area), mean = mean(area),
       s = sd(area), var = var(area),
       Q1 = quantile(area, 0.25), Q3 = quantile(area, 0.75)))


###################################################
### chunk number 26: bwplot1show eval=FALSE
###################################################
## bwplot(~ area, surfarea, xlab = "Surface area of Silica samples")


###################################################
### chunk number 27: bwplot1
###################################################
show(bwplot(~ area, surfarea, xlab = "Surface area of silica samples"))


###################################################
### chunk number 28: bwplot2show eval=FALSE
###################################################
## bwplot(batch ~ time, tablets)


###################################################
### chunk number 29: bwplot2
###################################################
show(bwplot(batch ~ time, tablets,
            xlab = "Lifetime of chlorine tablets (days)",
            ylab = "Batch of product"))


###################################################
### chunk number 30: strdhaze
###################################################
str(dhaze)
xtabs(~ treatment, dhaze)


###################################################
### chunk number 31: dhazebwplot
###################################################
show(bwplot(treatment ~ dhaze, dhaze,
            xlab = "Change in haze after 150 cycles of abrasion",
            ylab = "Surface treatment"))


###################################################
### chunk number 32: dhbw2show eval=FALSE
###################################################
## bwplot(reorder(treatment, dhaze) ~ dhaze, dhaze)


###################################################
### chunk number 33: dhazebwplot2
###################################################
show(bwplot(reorder(treatment, dhaze) ~ dhaze, dhaze,
            xlab = "Change in haze after 150 cycles of abrasion",
            ylab = "Surface treatment"))


###################################################
### chunk number 34: dhdotshow eval=FALSE
###################################################
## dotplot(reorder(treatment, dhaze) ~ dhaze, dhaze, pch = 21,
##         jitter.y = TRUE, type = c("p", "a"))


###################################################
### chunk number 35: dhazedotplot
###################################################
show(dotplot(reorder(treatment, dhaze) ~ dhaze, dhaze,
             pch = 21, jitter.y = TRUE, type = c("p", "a"),
             xlab = "Change in haze after 150 cycles of abrasion",
             ylab = "Surface treatment"))


###################################################
### chunk number 36: lwstr
###################################################
str(lw)  # Example 2.3.2
xtabs(~ comp2 + comp1, lw)


###################################################
### chunk number 37: 
###################################################
dotplot(reorder(comp1, lw) ~ lw, lw,
        groups = comp2, type = c("p", "a"))


###################################################
### chunk number 38: lwintplot
###################################################
show(dotplot(reorder(comp1, lw) ~ lw, lw, groups = comp2,
             type = c("p", "a"),
             xlab = "Long wave (LW) appearance measure",
             ylab = "Component 1",
             auto.key = list(columns = 3, lines = TRUE)))


