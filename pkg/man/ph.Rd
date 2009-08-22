\name{ph}
\Rdversion{1.1}
\alias{ph}
\docType{data}
\title{
  pH measurements in a chemical process
}
\description{
  pH measurements in a chemical process
}
\format{
  A data frame with 35 observations on the following 2 variables.
  \describe{
    \item{\code{batch}}{a factor with levels \code{A}, \code{B} and
      \code{C}}
    \item{\code{ph}}{a numeric vector}
  }
}
\details{
  For each of three batches of material numerous pH readings were taken
  over the course of the production process. For this material the pH
  should be consistent throughout production.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(ph)
summary(ph)
dotplot(batch ~ ph, ph, pch = 21, jitter.y = TRUE)
ph$obs <- as.integer(gl(12, 1, len = 35))
xyplot(ph ~ obs|batch, ph, type = c("g","b"), layout = c(1,3),
       strip = FALSE, strip.left = TRUE, xlab = NULL)
}
\keyword{datasets}
