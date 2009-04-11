\name{wash}
\Rdversion{1.1}
\alias{wash}
\docType{data}
\title{
  Appearance of washed and unwashed panels
}
\description{
  Appearance ratings of washed and unwashed panels
}
\format{
  A data frame with 36 observations on the following 3 variables.
  \describe{
    \item{\code{type}}{type of treatment - a factor with levels
      \code{NW} (not washed) and \code{W} (washed)}
    \item{\code{time1}}{initial rating}
    \item{\code{time2}}{rating after 2 weeks}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(wash)
summary(wash)
show(plt1 <- xyplot(time2 ~ time1, wash, groups = type,
       auto.key = list(columns = 2, lines = TRUE),
       aspect = "iso",
       type = c("g","p","smooth"),
       xlab  = "Initial rating",
       ylab = "Rating after 2 weeks",
       jitter.x = TRUE, jitter.y = TRUE))
tmd(plt1)
densityplot(~ I(time2 - time1), wash, groups = type,
            auto.key = list(columns = 2, lines = TRUE),
            xlab = "Change in rating after two weeks")
}
\keyword{datasets}
