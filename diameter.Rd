\name{diameter}
\Rdversion{1.1}
\alias{diameter}
\docType{data}
\title{
  Diameter of product
}
\description{
  The diameter of tracking balls for computer mice from two different
  production lines.  The nominal diameter of the ball is 2 cm.
}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{line}}{a factor with levels \code{A} \code{B}}
    \item{\code{diameter}}{diameter of the tracking ball (cm.)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(diameter)
dotplot(line ~ diameter, diameter, type = c("p","a"),
        jitter.y = TRUE, aspect = 0.2, ylab = "Line",
        xlab = "Diameter of tracking ball (cm.)")
bwplot(line ~ diameter, diameter, type = c("p","a"),
       jitter.y = TRUE, aspect = 0.2, ylab = "Line",
       xlab = "Diameter of tracking ball (cm.)")
}
\keyword{datasets}
