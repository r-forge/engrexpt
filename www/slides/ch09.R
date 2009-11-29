###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: strsep
###################################################
str(separate)


###################################################
### chunk number 3: separateplt
###################################################
print(dotplot(silica ~ y|temp,separate,groups=time,type=c("p","a"),
              ylab = "Silica level", layout = c(1,2),
              xlab = "Electrical resistance (panels are temperature levels)",
              strip = FALSE, strip.left = TRUE,
              auto.key = list(space = "right", title = "Time",
              lines = TRUE)))


###################################################
### chunk number 4: fm1
###################################################
summary(fm1 <- aov(y ~ time * temp * silica, separate))


###################################################
### chunk number 5: fm1a
###################################################
summary(fm1a <- aov(y ~ time * silica, separate,
                    subset = temp == "High"))
summary(fm1b <- aov(y ~ time + silica, separate,
                    subset = temp == "High"))


###################################################
### chunk number 6: xmp9.1.2
###################################################
str(defect <-
    data.frame(num = c(9,9,5,5,7,8,2,3,8,7,4,4),
               op = gl(3,4),
               app = gl(2,2,12,labels = c("A","B")),
               size = gl(2,1,12)))


###################################################
### chunk number 7: ftable
###################################################
ftable(xtabs(num ~ op+size+app, defect)) # compare Table 9.7
summary(fm2 <- aov(num ~ op + size * app, defect))


###################################################
### chunk number 8: fm34
###################################################
summary(fm3 <- aov(num ~ op + size + app, defect))
summary(fm4 <- aov(num ~ op + app, defect))


###################################################
### chunk number 9: defoam
###################################################
str(defoam)


###################################################
### chunk number 10: defoamplt
###################################################
print(dotplot(pH ~ height|conc, defoam, groups = temp,
              xlab = "Height of solution", type = c("p","a"),
              auto.key = list(columns = 3, lines = TRUE),
              layout = c(1,3), aspect = 0.35,
              strip = FALSE, strip.left = TRUE,
              ylab = "pH within concentration"),
      split = c(1,1,2,1), more = TRUE)
print(dotplot(pH ~ height|temp, defoam, groups = conc,
              xlab = "Height of solution", type = c("p","a"),
              auto.key = list(columns = 3, lines = TRUE),
              layout = c(1,3), aspect = 0.35,
              strip = FALSE, strip.left = TRUE,
              ylab = "pH within temperature"),
      split = c(2,1,2,1))


###################################################
### chunk number 11: fm56
###################################################
summary(fm5 <- aov(height ~ (conc + pH + temp)^2, defoam))
summary(fm6 <- aov(height ~ (pH + temp)^2, defoam))


###################################################
### chunk number 12: opcontr
###################################################
options(contrasts = c("contr.treatment", "contr.helmert"))
str(dents <- data.frame(ok = c(917,600,953,750,735,567,977,647),
                        A = gl(2,1,8,ord=1), B = gl(2,2,8,ord=1),
                        C = gl(2,4,ord=1)))


###################################################
### chunk number 13: dentplt
###################################################
print(dotplot(A ~ ok|B, dents, groups = C,
              xlab = "Number of dent-free assemblies", type = c("p","a"),
              auto.key = list(columns = 2, lines = TRUE),
              layout = c(1,2), aspect = 0.7,
              strip = FALSE, strip.left = TRUE,
              ylab = "film thickness within oil mixture"),
      split = c(1,1,2,1), more = TRUE)
print(dotplot(A ~ ok|C, dents, groups = B,
              xlab = "Number of dent-free assemblies", type = c("p","a"),
              auto.key = list(columns = 2, lines = TRUE),
              layout = c(1,2), aspect = 0.7,
              strip = FALSE, strip.left = TRUE,
              ylab = "film thickness within glove type"),
      split = c(2,1,2,1))


###################################################
### chunk number 14: fm7
###################################################
summary(fm7 <- aov(ok ~ (A+B+C)^2, dents))


###################################################
### chunk number 15: fm8
###################################################
summary(fm8 <- aov(ok ~ A+B+C, dents))


###################################################
### chunk number 16: fm9
###################################################
summary(fm9 <- aov(ok ~ A+B, dents))


###################################################
### chunk number 17: sumlm9 eval=FALSE
###################################################
## summary.lm(fm9)


###################################################
### chunk number 18: sumlm9res
###################################################
cat(paste(capture.output(summary.lm(fm9))[-(1:8)], collapse = "\n"))


###################################################
### chunk number 19: fitted
###################################################
fitted(fm9)


###################################################
### chunk number 20: opw
###################################################
op <- options(width=100)


###################################################
### chunk number 21: coeffm10
###################################################
(cc <- coef(fm10 <- lm(ok ~ A * B * C, dents))[-1])


###################################################
### chunk number 22: probplt
###################################################
print(qqmath(~ cc, aspect = 1,
             xlab = "Standard normal quantiles",
             ylab = "Estimated coefficients",
             panel = function(...){
                 panel.grid(h = -1, v = -1)
                 panel.qqmath(...)
                 panel.qqmathline(...)
             }))


###################################################
### chunk number 23: halfnormal
###################################################
print(qqmath( ~ abs(cc), aspect = 1, type = c("g","p"),
             distribution = function(p) sqrt(qchisq(p, df = 1))))


###################################################
### chunk number 24: xmp932
###################################################
xmp932 <- data.frame(A = gl(2,1,8,ord=1), B = gl(2,2,8,ord=1),
                     C = gl(2,4,ord=1))
model.matrix(~ A * B * C, xmp932)


###################################################
### chunk number 25: xmp932obs
###################################################
xmp932 <- within(xmp932, {
    D <- ordered(c(1,2,2,1,2,1,1,2))
    y <- c(3.6,10,8,3.2,7.6,3.2,3.7,6.0)
})
(cc2 <- coef(fm11 <- lm(y ~ A * B * C, xmp932))[-1])


###################################################
### chunk number 26: probplt2
###################################################
print(qqmath(~ cc2, aspect = 1,
             xlab = "Standard normal quantiles",
             ylab = "Estimated coefficients",
             panel = function(...){
                 panel.grid(h = -1, v = -1)
                 panel.qqmath(...)
                 panel.qqmathline(...)
             }))


