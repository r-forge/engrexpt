###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: uvcoatin
###################################################
str(uvcoatin)
head(uvcoatin)


###################################################
### chunk number 3: uvcoatinplot
###################################################
print(xyplot(a ~ b, uvcoatin, type = c("g","p"), aspect = 1),
      split = c(1,1,3,1), more = TRUE)
print(tmd(xyplot(a ~ b, uvcoatin)),
      split = c(2,1,3,1), more = TRUE)
print(qqmath(~ a-b, uvcoatin, aspect = 1, type = c("g","p"),
             xlab = "Standard normal quantiles"),
      split = c(3,1,3,1))


###################################################
### chunk number 4: uvttest
###################################################
with(uvcoatin, t.test(a, b, alt = "g", paired = TRUE))
with(uvcoatin, t.test(a, b, pair = 1))$conf.int


###################################################
### chunk number 5: strrail
###################################################
str(railcar3)


###################################################
### chunk number 6: railcardot
###################################################
print(dotplot(type ~ moisture, railcar3, type = c("p","a"),
              jitter.y = TRUE, pch = 21,
              xlab = "Moisture level of product at destination"))


###################################################
### chunk number 7: railttest
###################################################
t.test(moisture ~ type, railcar3, alt = "g")


###################################################
### chunk number 8: assayplot
###################################################
print(dotplot(process ~ yield, assay, type = c("p","a"),
              jitter.y = TRUE, pch = 21))


###################################################
### chunk number 9: assayt
###################################################
t.test(yield ~ process, assay)


###################################################
### chunk number 10: propt1
###################################################
prop.test(c(8, 14), c(500, 500))$conf.int


###################################################
### chunk number 11: propt2
###################################################
prop.test(c(26, 17), c(312, 329))


###################################################
### chunk number 12: propt3
###################################################
sqrt(prop.test(c(26, 17), c(312, 329))$statistic) # with cont. corr.
sqrt(prop.test(c(26, 17), c(312, 329), corr = 0)$statistic)


###################################################
### chunk number 13: powerprop
###################################################
power.prop.test(p1 = 0.45, p2 = 0.55, sig = 0.1, power = 0.9, alt = "one")


