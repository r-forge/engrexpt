###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: ex711data
###################################################
reac <-
    data.frame(yield = c(3.3, 4.0, 4.7,
                         4.0, 5.0, 4.3, 5.5,
                         5.3, 6.5, 6.4, 7.0, 7.7),
               cat = factor(rep(LETTERS[1:3], c(3,4,5))))
summary(fm1 <- aov(yield ~ cat, reac))


###################################################
### chunk number 3: reacplot
###################################################
print(dotplot(cat ~ yield, reac, type = c("a","p"), pch = 21, aspect = 0.3),
      pos = c(0,0,0.65,1), more = TRUE)
print(qqmath(~ yield, reac, groups = cat, aspect = 1.3,
             xlab = "Standard normal quantiles",
             auto.key = list(columns = 3)),
      pos = c(0.65,0,1,1))


###################################################
### chunk number 4: Tukeyfm1
###################################################
TukeyHSD(fm1)


###################################################
### chunk number 5: Tukeyplot
###################################################
op <- par(las = 1)
plot(TukeyHSD(fm1))


###################################################
### chunk number 6: resfm1
###################################################
print(xyplot(resid(fm1) ~ fitted(fm1), type = c("g","p"),
             xlab = "Fitted values", ylab = "Residuals"),
      pos = c(0,0,0.65,1), more = TRUE)
print(qqmath(~ resid(fm1), type = c("g", "p"), aspect = 1, ylab = NULL,
             xlab = "Standard normal quantiles"),
      pos = c(0.65,0,1,1))


###################################################
### chunk number 7: qqmathshow eval=FALSE
###################################################
## qqmath(~ wear, tennis, groups = type, type = c("g","p"),
##        panel = function(...){
##            panel.qqmath(...)
##            panel.qqmathline(...)})


###################################################
### chunk number 8: tennisplots
###################################################
print(dotplot(reorder(type, wear) ~ wear, tennis, pch = 21, type = c("p","a"),
              xlab = "Number of serves to wear off reference mark",
              jitter.y = TRUE, ylab = "Type of ball"),
      pos = c(0,0,0.7,1), more = TRUE)
print(qqmath(~ wear, tennis, groups = type, type = c("g","p"),
             auto.key = list(space = "right", points = FALSE, lines = TRUE),
             panel = function(...){panel.qqmath(...);panel.qqmathline(...)},
             aspect = 1.7, xlab = "Standard normal quantiles"),
      pos = c(0.7,0,1,1))


###################################################
### chunk number 9: tennisanova
###################################################
summary(fm2 <- aov(wear ~ type, tennis))


###################################################
### chunk number 10: fm2multcomp
###################################################
opar <- par(las = 1)
plot(TukeyHSD(fm2, order = TRUE))


###################################################
### chunk number 11: aerosol
###################################################
aerosol <- data.frame(cover = c(2.1,1.9,1.8,2.2,
                      4.7,3.6,3.9,3.8, 6.4,8.5,7.9,8.8),
                      brand = gl(3, 4, labels = LETTERS[1:3]))


###################################################
### chunk number 12: aerosolplot
###################################################
print(dotplot(reorder(brand, cover) ~ cover, aerosol,
              pch = 21, type = c("p","a"),
              xlab = "Covering capabilities (oz./sq. ft.) of gloss white aerosol",
              jitter.y = TRUE, ylab = "Brand"),
      pos = c(0,0,0.7,1), more = TRUE)
print(qqmath(~ cover, aerosol, groups = brand, type = c("g","p"),
             auto.key = list(space = "right", points = FALSE, lines = TRUE),
             panel = function(...){panel.qqmath(...);panel.qqmathline(...)},
             aspect = 1.7, xlab = "Standard normal quantiles"),
      pos = c(0.7,0,1,1))


###################################################
### chunk number 13: aerosollogplot
###################################################
print(dotplot(reorder(brand, cover) ~ log(cover), aerosol,
              pch = 21, type = c("p","a"),
              xlab = "log(covering capability (oz./sq. ft.)) of gloss white aerosol",
              jitter.y = TRUE, ylab = "Brand"),
      pos = c(0,0,0.7,1), more = TRUE)
print(qqmath(~ log(cover), aerosol, groups = brand, type = c("g","p"),
             auto.key = list(space = "right", points = FALSE, lines = TRUE),
             panel = function(...){panel.qqmath(...);panel.qqmathline(...)},
             aspect = 1.7, xlab = "Standard normal quantiles"),
      pos = c(0.7,0,1,1))


###################################################
### chunk number 14: aerosolanova
###################################################
summary(fm3 <- aov(cover ~ brand, aerosol))
summary(fm3a <- aov(log(cover) ~ brand, aerosol))
TukeyHSD(fm3a)


###################################################
### chunk number 15: boxcox
###################################################
library(MASS)
boxcox(fm3)


###################################################
### chunk number 16: tennissqrtplots
###################################################
print(dotplot(reorder(type, sqrt(wear)) ~ sqrt(wear), tennis,
              pch = 21, type = c("p","a"),
              xlab = "Square root of number of serves to wear off reference mark",
              jitter.y = TRUE, ylab = "Type of ball"),
      pos = c(0,0,0.7,1), more = TRUE)
print(qqmath(~ sqrt(wear), tennis, groups = type, type = c("g","p"),
             auto.key = list(space = "right", points = FALSE, lines = TRUE),
             panel = function(...){panel.qqmath(...);panel.qqmathline(...)},
             aspect = 1.7, xlab = "Standard normal quantiles"),
      pos = c(0.7,0,1,1))


###################################################
### chunk number 17: tennisanova
###################################################
summary(fm2a <- aov(sqrt(wear) ~ type, tennis))


###################################################
### chunk number 18: tennissqrtTukey
###################################################
TukeyHSD(fm2a, ordered = TRUE)


###################################################
### chunk number 19: tennissqrtTukeyplot
###################################################
opar <- par(las = 1)
plot(TukeyHSD(fm2a, ordered = TRUE))


