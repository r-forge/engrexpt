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


###################################################
### chunk number 3: uvcoatinplot
###################################################
print(xyplot(a ~ b, uvcoatin, type = c("g","p")),
      split = c(1,1,2,1), more = TRUE)
print(tmd(xyplot(a ~ b, uvcoatin)),
      split = c(2,1,2,1), more = TRUE)


###################################################
### chunk number 4: uvttest
###################################################
with(uvcoatin, t.test(a, b, alt = "g", paired = TRUE))


