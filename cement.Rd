\name{cement}
\Rdversion{1.1}
\alias{cement}
\docType{data}
\title{
  Drying times of concrete
}
\description{
  Drying times of different concrete mixes used in a sidewalk.  Mix
  \sQuote{\code{A}} is the conventional mix.  Mixes \sQuote{\code{B}}
  and \sQuote{\code{C}} are experimental, and more expensive, mixes.
}
\format{
  A data frame with 19 observations on the following 2 variables.
  \describe{
    \item{\code{type}}{concrete mix - a factor with levels \code{A},
      \code{B} and \code{C}}
    \item{\code{time}}{time (hr) until concrete is dry enough to walk
      on}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cement)
dotplot(reorder(type, time) ~ time, cement, type = c("p","a"),
        jitter.y = TRUE, ylab = "Mix type",
        xlab = "Time until concrete is dry enough to walk on (hours)")
qqmath(~ time, cement, groups = type, aspect = 'xy',
       xlab = "Standard normal quantiles",
       type = c("g","p"),
       panel=function(...)
   {
       panel.qqmathline(..., alpha = 0.5, lty = 2)
       panel.qqmath(...)
   },
       ylab = "Time until concrete is dry enough to walk on (hours)",
       auto.key = list(space = "right", title = "Type", lines = TRUE))
summary(fm1 <- aov(time ~ type, cement))
TukeyHSD(fm1)
}
\keyword{datasets}
