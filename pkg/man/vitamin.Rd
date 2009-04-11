\name{vitamin}
\Rdversion{1.1}
\alias{vitamin}
\docType{data}
\title{
  Calcium levels before and after vitamin supplement
}
\description{
  Calcium levels before and after vitamin supplement
}
\format{
  A data frame with 49 observations on the following 3 variables.
  \describe{
    \item{\code{treatment}}{formulation - a factor with levels \code{A},
      \code{B} and \code{C}}
    \item{\code{before}}{Calcium level before the vitamin}
    \item{\code{after}}{Calcium level after the vitamin}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(vitamin)
summary(vitamin)
show(plt1 <- xyplot(after ~ before, vitamin, groups = treatment,
       auto.key = list(space = "right", title = "Formulation", lines = TRUE),
       aspect = "iso",
       type = c("g","p","smooth"),
       xlab  = "Calcium level before the vitamin",
       ylab = "Calcium level after the vitamin"))
tmd(plt1, aspect = 1)
densityplot(~ I(after - before), vitamin, groups = treatment,
            auto.key = list(columns = 3, lines = TRUE),
            xlab = "Change in calcium level after taking vitamin")
}
\keyword{datasets}
