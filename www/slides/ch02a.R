###################################################
### chunk number 1: preliminaries
###################################################
options(width=60)
library(EngrExpt)
options(show.signif.stars = FALSE)
#lattice.options(default.theme = function() standard.theme(color=FALSE))


###################################################
### chunk number 2: simple
###################################################
5 - 1 + 10
7 * 10 / 2
exp(-2.19)
pi
sin(2 * pi/3)


###################################################
### chunk number 3: ls
###################################################
x <- 5  # assignments are not echoed
ls()
ls.str()
rm(x)
ls()


###################################################
### chunk number 4: subscripts
###################################################
(x <- 0:19) # the redundant parens cause printing of x
x[5]
str(y <- x + runif(20, min = 10, max = 20))


###################################################
### chunk number 5: installEngrExpt eval=FALSE
###################################################
## install.packages("EngrExpt")


###################################################
### chunk number 6: libraryEngrExpt eval=FALSE
###################################################
## library(EngrExpt)


###################################################
### chunk number 7: strodor
###################################################
str(odor)


###################################################
### chunk number 8: readcsv eval=FALSE
###################################################
## mydata <- read.csv(file.choose())


###################################################
### chunk number 9: readcsv eval=FALSE
###################################################
## mydata <- read.delim(file.choose())


###################################################
### chunk number 10: Formaldehyde
###################################################
str(Formaldehyde)
summary(Formaldehyde)
Formaldehyde


###################################################
### chunk number 11: InsectSprays
###################################################
str(InsectSprays)
summary(InsectSprays)
head(InsectSprays)


###################################################
### chunk number 12: saverestore
###################################################
sprays <- InsectSprays
save(sprays, file = "sprays.rda")
rm(sprays)
ls.str()
load("sprays.rda")
names(sprays)


###################################################
### chunk number 13: dollarop
###################################################
Formaldehyde$carb


###################################################
### chunk number 14: dollaropleft
###################################################
sprays$sqrtcount <- sqrt(sprays$count)
names(sprays)


###################################################
### chunk number 15: dollaropleftNULL
###################################################
sprays$sqrtcount <- NULL
names(sprays)


###################################################
### chunk number 16: formalfoo
###################################################
Formaldehyde$carb * Formaldehyde$optden
with(Formaldehyde, carb * optden)


###################################################
### chunk number 17: within
###################################################
sprays <- within(sprays, sqrtcount <- sqrt(count))
str(sprays)


###################################################
### chunk number 18: sprays
###################################################
str(sprays <- within(InsectSprays, spray <- as.integer(spray)))
str(sprays <- within(sprays, spray <- factor(spray, labels = LETTERS[1:6])))


###################################################
### chunk number 19: sprayA
###################################################
str(sprayA <- subset(sprays, spray == "A"))


###################################################
### chunk number 20: xtabssprays
###################################################
xtabs( ~ spray, sprayA)
xtabs( ~ spray, sprayA, drop = TRUE)


###################################################
### chunk number 21: sprayAdrop
###################################################
str(sprayA <- within(sprayA, spray <- factor(spray)))
xtabs( ~ spray, sprayA)


###################################################
### chunk number 22: sprayDEF
###################################################
str(sprayDEF <- subset(sprays, spray %in% c("D","E","F")))


###################################################
### chunk number 23: na
###################################################
(x <- c(10, 20, NA, 4, NA, 2))
sum(x)
is.na(x)
sum(x[!is.na(x)])


###################################################
### chunk number 24: na.rm
###################################################
sum(x, na.rm = TRUE)
(x <- c(0, 1, NA, 0/0, Inf))
sum(x, na.rm = TRUE)


